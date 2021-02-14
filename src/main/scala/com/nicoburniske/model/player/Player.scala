package com.nicoburniske.model.player

import com.nicoburniske.model.blackjack.{GameAction, GamePlayer, Hand}


trait Player {
  def gameStart(id: Int): Boolean
  def handStart(info: PlayerView): Int
  def informOtherPlayerMove(info: PlayerView): Boolean
  def getHandAction(playerInfo: GamePlayer, dealer: Hand): GameAction
  def informHandResult(won: Boolean, playerHand: Hand, dealerHand: Hand): Boolean
  def gameEnd(endScore: Int): Boolean
}
