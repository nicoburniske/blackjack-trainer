package com.nicoburniske.model.player

import com.nicoburniske.model.blackjack.{GameAction, GamePlayer, Hand}
import com.nicoburniske.model.ref.BlackjackHistory


trait Player {
  def gameStart(id: Int): Boolean
  def handStart(info: PlayerView): Option[Int]
  def informOtherPlayerMove(info: PlayerView): Boolean
  def getHandAction(playerInfo: GamePlayer, dealer: Hand): GameAction
  def informHandResult(won: Int, playerHand: Hand, dealerHand: Hand): Boolean
  def gameEnd(endScore: Int, history: BlackjackHistory): Boolean
}

