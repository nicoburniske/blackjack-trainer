package com.nicoburniske.model.ref

import com.nicoburniske.model.blackjack._
import com.nicoburniske.model.player.{PlayerView, TablePlayer}
import com.nicoburniske.model.strat.DealerStrategy

import scala.annotation.tailrec
import scala.collection.immutable.ListMap

case class BlackjackState(players: List[TablePlayer], model: BlackjackModel, history: List[BlackjackHistory]) {
  require(players.length == model.players.length)
  val numPlayers: Int = players.length

  def removePlayer(index: Int): BlackjackState = {
    def removeIndexFromList[T](index: Int, list: List[T]): List[T] = {
      list.take(index) ++ list.drop(index + 1)
    }

    val newPlayers = removeIndexFromList(index, this.players)
    val newModel = this.model.removePlayer(index)
    val newHistory = removeIndexFromList(index, this.history)

    BlackjackState(newPlayers, newModel, newHistory)
  }

  def addChoice(playerIndex: Int, choice: PlayerAction): BlackjackState = {
    val playerHistory = this.history(playerIndex)
    this.copy(history = this.history.updated(playerIndex, playerHistory.addChoice(this.model, choice)))
  }

  def addResult(playerIndex: Int, result: Int): BlackjackState = {
    val playerHistory = this.history(playerIndex)
    this.copy(history = this.history.updated(playerIndex, playerHistory.addHandResult(result)))
  }

  def getPlayers(index: Int): (TablePlayer, GamePlayer) = {
    require(index >= 0 && index < numPlayers)
    (this.players(index), this.model.players(index))
  }
}

case class BlackjackHistory(choices: ListMap[BlackjackModel, PlayerAction] = ListMap.empty, results: List[Int] = Nil) {
  val winCount: Int = results.count(_ > 0)
  val lossCount: Int = results.count(_ < 0)
  val tieCount: Int = results.count(_ == 0)

  def addHandResult(won: Int): BlackjackHistory = {
    BlackjackHistory(this.choices, won :: this.results)
  }

  def addChoice(state: BlackjackModel, choice: PlayerAction): BlackjackHistory = {
    BlackjackHistory(this.choices + (state -> choice), this.results)
  }
}

object BlackjackTable {

  def playGame(players: List[TablePlayer], strat: DealerStrategy): List[BlackjackHistory] = {
    val initialState = this.initState(players)
    players.zipWithIndex.map { case (player, index) => player.gameStart() }
    val endState = this.playRound(initialState, strat)
    endState.history
  }

  @tailrec
  def playRound(state: BlackjackState, strat: DealerStrategy): BlackjackState = {
    if (state.model.isOver) {
      state
    } else {
      val startHand = state.model.startHand
      val afterBets = this.removeQuittersAndUpdateBets(state.copy(model = startHand))
      val afterPlayersMoveState = (0 until afterBets.numPlayers).foldLeft(afterBets) {
        case (currState, index: Int) => this.singleTurn(currState, index)
      }
      val afterDealer = drawDealer(afterPlayersMoveState, strat)
      playRound(filterLosers(informPlayers(afterDealer)), strat)
    }
  }

  def removeQuittersAndUpdateBets(state: BlackjackState): BlackjackState = {
    val newState = (0 until state.numPlayers).foldLeft(state)((currentState, index) => {
      val (player, gamePlayer) = currentState.getPlayers(index)
      player.handStart(gamePlayer.score) match {
        case Some(bet) =>
          if (gamePlayer.validBet(bet)) {
            val newPlayer = GamePlayer(gamePlayer.hand, gamePlayer.score - bet, Some(bet))
            val newPlayers = currentState.model.players.updated(index, newPlayer)
            val newModel = currentState.model.updatePlayers(newPlayers)
            currentState.copy(model = newModel)
          } else {
            player.gameEnd(gamePlayer.score, currentState.history(index))
            currentState.removePlayer(index)
          }
        case None =>
          player.gameEnd(gamePlayer.score, currentState.history(index))
          currentState.removePlayer(index)
      }
    })
    // Update players of their (possibly) new positions. TODO: determine if this should only happen post removal
    newState.players.zipWithIndex.foreach { case (player, index) => player.setPlayerPosition(index) }
    newState
  }


  def filterLosers(state: BlackjackState): BlackjackState = {
    (0 until state.numPlayers).foldLeft(state) { (currentState, index) =>
      val (player, gamePlayer) = currentState.getPlayers(index)
      if (gamePlayer.isBroke) {
        player.gameEnd(gamePlayer.score, state.history(index))
        currentState.removePlayer(index)
      } else {
        currentState
      }
    }
  }

  def informPlayers(state: BlackjackState): BlackjackState = {
    val dealer = state.model.dealerHand // show complete hand
    (0 until state.numPlayers).foldLeft(state)((currentState, index) => {
      val (player, gamePlayer) = currentState.getPlayers(index)
      val won = gamePlayer.hand.winsOver(dealer).get
      player.informHandResult(won, gamePlayer.hand, dealer)
      val recordResult = currentState.addResult(index, won)
      val newPlayer = gamePlayer.updateScore(won)
      val gamePlayers = recordResult.model.players.updated(index, newPlayer)
      recordResult.copy(model = recordResult.model.updatePlayers(gamePlayers))
    })
  }

  @tailrec
  def drawDealer(state: BlackjackState, strat: DealerStrategy): BlackjackState = {
    if (!state.model.dealerHand.canHit) {
      state
    } else {
      strat.takeTurn(state.model) match {
        case Some(Stand) => state
        case Some(Hit) =>
          state.model.dealerAction(Hit) match {
            case Some(newModel) =>
              val newState = state.copy(model = newModel)
              drawDealer(newState, strat)
            case None => state
          }
        case None => state
      }
    }

  }

  def validAction(model: BlackjackModel, action: PlayerAction, index: Int): Option[BlackjackModel] = {
    model.playerAction(index, action)
  }

  @tailrec
  def singleTurn(state: BlackjackState, playerIndex: Int): BlackjackState = {
    val otherPlayers = state.players.zipWithIndex.filter(tuple => tuple._2 != playerIndex).map(_._1)
    val (currentPlayer, currentGamePlayer) = state.getPlayers(playerIndex)
    if (!currentGamePlayer.hand.canHit) {
      state
    } else {
      val action = currentPlayer.getHandAction(currentGamePlayer, state.model.dealerHand.hideAllButFirst)
      val newState = state.addChoice(playerIndex, action)
      state.model.playerAction(playerIndex, action) match {
        case Some(newModel) =>
          otherPlayers.map(_.informOtherPlayerMove(toPlayerView(newModel)))
          action match {
            case Stand =>
              newState.copy(model = newModel)
            case Hit =>
              this.singleTurn(newState.copy(model = newModel), playerIndex)
            case Double =>
              newState.copy(model = newModel)
            case Split =>
              throw new IllegalStateException("Split functionality has not been implemented yet")
            // TODO finish processing all gameactions
          }
        case None =>
          // TODO: remove the player from the state?
          throw new IllegalStateException("Invalid action requested")
      }
    }
  }

  private def initState(players: List[TablePlayer]): BlackjackState = {
    val gamePlayers = players.map(_ => GamePlayer(Hand(), 100, None))
    val playerHistory = players.map(_ => BlackjackHistory())
    val blackjack = new BlackjackModel(Hand(), gamePlayers, Deck(6), 6)
    BlackjackState(players, blackjack, playerHistory)
  }

  private def toPlayerView(model: BlackjackModel): PlayerView = {
    PlayerView(model.dealerHand.hideAllButFirst, model.players)
  }
}
