package com.nicoburniske.model.table

import com.nicoburniske.model.blackjack._
import com.nicoburniske.model.player.{PlayerView, TablePlayer}
import com.nicoburniske.model.strat.DealerStrategy

import scala.annotation.tailrec
import scala.collection.immutable.ListMap

/**
 * Represents the state of a table for Blackjack.
 * The player at index ii has their history stored at index ii
 *
 * @param players the players at the table
 * @param model   the state of the game
 * @param history the history for each player at the table
 */
case class BlackjackTable(players: List[TablePlayer], model: BlackjackModel, history: List[BlackjackHistory]) {
  require(players.length == model.players.length)
  val numPlayers: Int = players.length

  /**
   * Removes a player from the state.
   *
   * @param index the player to remove
   * @return the new State
   */
  def removePlayer(index: Int): BlackjackTable = {
    def removeIndexFromList[T](index: Int, list: List[T]): List[T] = {
      list.take(index) ++ list.drop(index + 1)
    }

    val newPlayers = removeIndexFromList(index, this.players)
    val newModel = this.model.removePlayer(index)
    val newHistory = removeIndexFromList(index, this.history)

    BlackjackTable(newPlayers, newModel, newHistory)
  }

  /**
   * Adds a choice to the given player's index.
   *
   * @param playerIndex the index of the player
   * @param choice      the choice to record
   * @return the updated state
   */
  def addChoice(playerIndex: Int, choice: PlayerAction): BlackjackTable = {
    val playerHistory = this.history(playerIndex)
    this.copy(history = this.history.updated(playerIndex, playerHistory.addChoice(this.model, choice)))
  }

  /**
   * Adds a hand result to the player's history.
   *
   * @param playerIndex the index of the player
   * @param result      the result to record
   * @return the updated state
   */
  def addResult(playerIndex: Int, result: Int): BlackjackTable = {
    val playerHistory = this.history(playerIndex)
    this.copy(history = this.history.updated(playerIndex, playerHistory.addHandResult(result)))
  }

  /**
   * Retrieves a the TablePlayer and corresponding GamePlayer.
   *
   * @param index the index of the player
   * @return the tuple of corresponding to the player at that index
   */
  def getPlayers(index: Int): (TablePlayer, GamePlayer) = {
    require(index >= 0 && index < this.numPlayers)
    (this.players(index), this.model.players(index))
  }
}

/**
 * Represents the history of past choices and results in the game of Blackjack by a player.
 *
 * @param choices a Map of game state to the choice made by the player
 * @param results the results of previous hands played by the player
 */
case class BlackjackHistory(choices: ListMap[BlackjackModel, PlayerAction] = ListMap.empty, results: List[Int] = Nil) {
  val winCount: Int = results.count(_ > 0)
  val lossCount: Int = results.count(_ < 0)
  val tieCount: Int = results.count(_ == 0)

  def addHandResult(won: Int): BlackjackHistory = {
    this.copy(results = won :: this.results)
  }

  def addChoice(state: BlackjackModel, choice: PlayerAction): BlackjackHistory = {
    this.copy(choices = this.choices + (state -> choice))
  }
}

object Blackjack {

  /**
   * Runs a game of Blackjack between the players.
   *
   * @param players the players that will play at a single table of blackjack
   * @param strat   the strategy of the dealer
   * @return the history for all players at the table
   */
  def playGame(players: List[TablePlayer], strat: DealerStrategy): List[BlackjackHistory] = {
    val initialState = this.initState(players)
    players.zipWithIndex.map { case (player, index) => player.gameStart() }
    val endState = this.playRound(initialState, strat)
    endState.history
  }


  /**
   * Runs a round of Blackjack.
   *
   * Each player must:
   * - place bets or quit
   * - draw cards until they bust or Stand
   *
   * Then the dealer draws cards according to the provided DealerStrategy
   * Then each player is notified of the result of the hand
   * Then the losers are identified, notified of their performance, and removed from the Table
   *
   * @param state the current state of the table
   * @param strat the strategy for the dealer
   * @return the state of the table after the round
   */
  @tailrec
  def playRound(state: BlackjackTable, strat: DealerStrategy): BlackjackTable = {
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

  /**
   * Queries every player for a bet. Players can also quit.
   *
   * If a player quits they will be removed from the table and notified of their performance.
   *
   * @param state the current state of the table
   * @return the updated state with quitters removed, and bets updated for each player
   */
  def removeQuittersAndUpdateBets(state: BlackjackTable): BlackjackTable = {
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


  /**
   * Removes losers and notifies them of their performance.
   *
   * @param state the current state
   * @return the updated state without the losers
   */
  def filterLosers(state: BlackjackTable): BlackjackTable = {
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

  def informPlayers(state: BlackjackTable): BlackjackTable = {
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

  /**
   * Populate the dealer's hand according to the provided DealerStrategy
   *
   * @param state the state of the Table
   * @param strat the strategy used to draw cards for a dealer
   * @return the updated state containing the new Dealer hand
   */
  @tailrec
  def drawDealer(state: BlackjackTable, strat: DealerStrategy): BlackjackTable = {
    if (!state.model.dealerHand.canHit) {
      state
    } else {
      strat.takeTurn(state.model.dealerHand) match {
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

  /**
   * Run's a single player turn until they have either:
   * - Replied "Stand"
   * - Hit blackjack
   * - Gone bust
   *
   * - Also updates the other players with the new state of the table after every action
   * @param state the current state of the table
   * @param playerIndex the index of the player whose turn it is
   * @return the updated table state
   */
  @tailrec
  def singleTurn(state: BlackjackTable, playerIndex: Int): BlackjackTable = {
    val otherPlayers = state.players.zipWithIndex.filter(tuple => tuple._2 != playerIndex).map(_._1)
    val (currentPlayer, currentGamePlayer) = state.getPlayers(playerIndex)
    if (!currentGamePlayer.hand.canHit) {
      state
    } else {
      val action = currentPlayer.getHandAction(currentGamePlayer, state.model.dealerHand.hideAllButFirst)
      val newState = state.addChoice(playerIndex, action)
      state.model.playerAction(playerIndex, action) match {
        case Some(newModel) =>
          // Update other players of what the current player has done.
          val playerView = PlayerView(newModel.dealerHand.hideAllButFirst, newModel.players)
          otherPlayers.map(_.informOtherPlayerMove(playerView))
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

  /**
   * Initializes the BlackjackTable.
   *
   * @param players the players to include at the table
   * @return the initialized BlackjackTable
   */
  private def initState(players: List[TablePlayer]): BlackjackTable = {
    val gamePlayers = players.map(_ => GamePlayer(Hand(), 100, None))
    val playerHistory = players.map(_ => BlackjackHistory())
    val blackjack = new BlackjackModel(Hand(), gamePlayers, Shoe(6), 6)
    BlackjackTable(players, blackjack, playerHistory)
  }
}
