package com.nicoburniske.model.player

import com.nicoburniske.model.blackjack.{GamePlayer, Hand, PlayerAction}
import com.nicoburniske.model.table.BlackjackHistory

/**
 * Represents a player to be queried for actions and informed of results in a game of Blackjack.
 */
trait TablePlayer {
  /**
   * Signal that the game has started.
   *
   * @return T/F whether the player has acknowledged
   */
  def gameStart(): Boolean

  /**
   * Inform the player of their new position at the table
   *
   * @param position the player's position at the table
   * @return T/F whether the player has acknowledged
   */
  def setPlayerPosition(position: Int): Boolean

  /**
   * Signal that the hand has started.
   *
   * @param bankroll the amount of money the player has available to bet
   * @return the amount that player wishes to bet, or None if the player wants to quit
   */
  def handStart(bankroll: Int): Option[Int]

  /**
   * Inform the player of another player's move at the table.
   *
   * @param info the table state
   * @return T/F whether the player has acknowledged
   */
  def informOtherPlayerMove(info: PlayerView): Boolean

  /**
   * Requests a move from the player
   *
   * @param playerInfo the player's current state
   * @param dealer     the dealer's hand
   * @return the PlayerAction chosen by the player
   */
  def getHandAction(playerInfo: GamePlayer, dealer: Hand): PlayerAction

  /**
   * Informs the player of the result of the hand
   *
   * @param won        -1 if lost, 0 if tied, 1 if won
   * @param playerHand the player's hand
   * @param dealerHand the dealer's hand
   * @return T/F whether the player has acknowledged
   */
  def informHandResult(won: Int, playerHand: Hand, dealerHand: Hand): Boolean

  /**
   * Informs the player after they have left the table / quit the game.
   *
   * @param endScore the amount of money the player is leaving with
   * @param history  the given player's history
   * @return T/F whether the player has acknowledged
   */
  def gameEnd(endScore: Int, history: BlackjackHistory): Boolean
}

