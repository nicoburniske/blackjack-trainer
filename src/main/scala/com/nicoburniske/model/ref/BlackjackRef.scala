package com.nicoburniske.model.ref

import com.nicoburniske.model.blackjack._
import com.nicoburniske.model.player.{Player, PlayerView}
import com.nicoburniske.model.strat.CompleteStrategy

import scala.annotation.tailrec
import scala.collection.immutable.ListMap

case class BlackjackState(players: List[Player], model: BlackjackModel, history: List[BlackjackHistory]) {
  def removePlayer(index: Int): BlackjackState = {
    BlackjackState(this.players.take(index) ++ this.players.drop(index + 1), model, history)
  }

  def updateModel(newModel: BlackjackModel): BlackjackState = {
    BlackjackState(players, newModel, history)
  }

  def updatePlayers(newPlayers: Seq[Player]): BlackjackState = {
    // BlackjackState(newPlayers.toList, this.model, this.history)
    this.copy(players=newPlayers.toList)
  }

  def addChoice(playerIndex: Int, state: BlackjackModel, choice: GameAction): BlackjackState = {
    val playerHistory = this.history(playerIndex)
    this.copy(history = this.history.updated(playerIndex, playerHistory.addChoice(state, choice)))
  }
}

case class BlackjackHistory(choices: ListMap[BlackjackModel, GameAction] = ListMap.empty, results: List[Int]) {
  def addHandResult(won: Int): BlackjackHistory = {
    BlackjackHistory(this.choices, won :: this.results)
  }

  def addChoice(state: BlackjackModel, choice: GameAction): BlackjackHistory = {
    BlackjackHistory(this.choices + (state -> choice), this.results)
  }
}

object BlackjackRef {

  def playGame(players: List[Player], strat: CompleteStrategy): List[BlackjackHistory] = {
    val initialState = this.initState(players)
    players.zipWithIndex.map { case (player, index) => player.gameStart(index) }
    val endState = this.playRound(initialState, strat)
    endState.history
  }

  @tailrec
  def playRound(state: BlackjackState, strat: CompleteStrategy): BlackjackState = {
    if (state.model.isOver) {
      state
    } else {
      val newModel = state.model.startHand
      val newStatePlayerView = toPlayerView(newModel)
      val betPlayers = (state.players zip newModel.players).map { case (player, gamePlayer) =>
        // TODO: confirm that the input is valid. Could be betting over
        val bet = player.handStart(newStatePlayerView)
        GamePlayer(gamePlayer.hand, gamePlayer.score - bet, Some(bet))
      }
      val newState = state.updateModel(newModel.updatePlayers(betPlayers))
      val afterPlayersMoveState = newState.players.zipWithIndex.foldLeft(newState) {
        case (currState, (_: Player, index: Int)) =>
          val otherPlayers = newState.players.zipWithIndex.filter(tuple => tuple._2 != index).map(_._1)
          this.singleTurn(currState, index, otherPlayers)
      }

      val afterDealer = drawDealer(afterPlayersMoveState, strat)
      playRound(filterLosers(informPlayers(afterDealer)), strat)
    }
  }

  def filterLosers(state: BlackjackState): BlackjackState = {
    (state.players zip state.model.players).foreach { case (player, gamePlayer) =>
      if (gamePlayer.isBroke) {
        player.gameEnd(0)
      }
    }
    val remaining = (state.players zip state.model.players).filter { case (_, gamePlayer) =>
      !gamePlayer.isBroke
    }
    val newModel = state.model.updatePlayers(remaining.map(_._2))
    // update model and players remaining
    state.updateModel(newModel).updatePlayers(remaining.map(_._1))
  }

  def informPlayers(state: BlackjackState): BlackjackState = {
    val dealer = state.model.dealer
    val gamePlayers = (state.players zip state.model.players).map { case (player, gamePlayer) =>
      val won = gamePlayer.hand.winsOver(dealer).get
      player.informHandResult(won, gamePlayer.hand, dealer)
      gamePlayer.updateScore(won)
    }
    val newModel = state.model.updatePlayers(gamePlayers)
    state.updateModel(newModel)
  }

  def drawDealer(state: BlackjackState, strat: CompleteStrategy): BlackjackState = {
    @tailrec
    def continueTillStand(state: BlackjackState, last: GameAction): BlackjackState = {
      last match {
        case Stand => state
        case Hit =>
          strat.takeTurn(state.model) match {
            case Some(action) =>
              state.model.dealerAction(action) match {
                case Some(newModel) =>
                  continueTillStand(state.updateModel(newModel), action)
                case None => state
              }
            // TODO: log this?
            case None => state
          }
        case _ => state
      }
    }

    continueTillStand(state, strat.takeTurn(state.model).get)
  }

  def validAction(model: BlackjackModel, action: GameAction, index: Int): Option[BlackjackModel] = {
    model.playerAction(index, action)
  }

  @tailrec
  def singleTurn(state: BlackjackState, playerIndex: Int, otherPlayers: List[Player]): BlackjackState = {
    val currentGamePlayer = state.model.players(playerIndex)
    val currentPlayer = state.players(playerIndex)
    if (!currentGamePlayer.hand.canHit) {
      state
    } else {
      val action = currentPlayer.getHandAction(currentGamePlayer, state.model.dealer.hideAllButFirst)
      // state.history(playerIndex).copy()
      this.validAction(state.model, action, playerIndex) match {
        case Some(newState) =>
          otherPlayers.map(_.informOtherPlayerMove(toPlayerView(newState)))
          action match {
            case Stand =>
              state.updateModel(newState)
            case Hit =>
              this.singleTurn(state.updateModel(newState), playerIndex, otherPlayers)
            case Quit =>
              state.updateModel(newState).removePlayer(playerIndex)
            case _ =>
              state.updateModel(newState)
            // TODO finish processing all gameactions
          }
        case _ => throw new IllegalStateException("Invalid action requested")
      }
    }
  }

  private def initState(players: List[Player]): BlackjackState = {
    val gamePlayers = players.map(_ => GamePlayer(Hand(), 100, None))
    val blackjack = new BlackjackModel(Hand(), gamePlayers, Deck(6), 6)
    BlackjackState(players, blackjack, Nil)
  }

  private def toPlayerView(model: BlackjackModel): PlayerView = {
    PlayerView(model.dealer.hideAllButFirst, model.players)
  }
}
