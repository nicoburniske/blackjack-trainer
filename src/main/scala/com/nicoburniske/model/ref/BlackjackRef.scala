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

  def addChoice(playerIndex: Int, state: BlackjackModel, choice: GameAction): BlackjackState = {
    val playerHistory = this.history(playerIndex)
    this.copy(history = this.history.updated(playerIndex, playerHistory.addChoice(state, choice)))
  }
}

case class BlackjackHistory(choices: ListMap[BlackjackModel, GameAction] = ListMap.empty, results: List[Int] = Nil) {
  val winCount: Int = results.count(_ > 0)
  val lossCount: Int = results.count(_ < 0)
  val tieCount: Int = results.count(_ == 0)

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
        val bet = player.handStart(newStatePlayerView).get // TODO: ensure that the player hasn't quit
        GamePlayer(gamePlayer.hand, gamePlayer.score - bet, Some(bet))
      }
      val newState = state.copy(model = newModel.updatePlayers(betPlayers))
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
    (state.players zip state.model.players).zipWithIndex.foreach { case ((player, gamePlayer), index) =>
      if (gamePlayer.isBroke) {
        player.gameEnd(gamePlayer.score, state.history(index))
      }
    }
    val brokePlayers = state.model.players.zipWithIndex
      .filter { case (gamePlayer, index) => gamePlayer.isBroke }
      .map(_._2)

    val newPlayers = (state.players zip state.model.players).zipWithIndex
      .filter { case ((_, _), index) => !brokePlayers.contains(index) }
      .map(_._1)

    // update model and players remaining
    val newModel = state.model.updatePlayers(newPlayers.map(_._2))
    state.copy(model = newModel, players = newPlayers.map(_._1))
  }

  def informPlayers(state: BlackjackState): BlackjackState = {
    val dealer = state.model.dealer
    val gamePlayers = (state.players zip state.model.players).map { case (player, gamePlayer) =>
      val won = gamePlayer.hand.winsOver(dealer).get
      player.informHandResult(won, gamePlayer.hand, dealer)
      gamePlayer.updateScore(won)
    }
    state.copy(model = state.model.updatePlayers(gamePlayers))
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
                  continueTillStand(state.copy(model = newModel), action)
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
      val newState = state.addChoice(playerIndex, state.model, action)
      this.validAction(state.model, action, playerIndex) match {
        case Some(newModel) =>
          otherPlayers.map(_.informOtherPlayerMove(toPlayerView(newModel)))
          action match {
            case Stand =>
              newState.copy(model = newModel)
            case Hit =>
              this.singleTurn(newState.copy(model = newModel), playerIndex, otherPlayers)
            case Quit =>
              newState.copy(model = newModel).removePlayer(playerIndex)
            case _ =>
              newState.copy(model = newModel)
            // TODO finish processing all gameactions
          }
        case _ => throw new IllegalStateException("Invalid action requested")
      }
    }
  }

  private def initState(players: List[Player]): BlackjackState = {
    val gamePlayers = players.map(_ => GamePlayer(Hand(), 100, None))
    val playerHistory = players.map(_ => BlackjackHistory())
    val blackjack = new BlackjackModel(Hand(), gamePlayers, Deck(6), 6)
    BlackjackState(players, blackjack, playerHistory)
  }

  private def toPlayerView(model: BlackjackModel): PlayerView = {
    PlayerView(model.dealer.hideAllButFirst, model.players)
  }
}
