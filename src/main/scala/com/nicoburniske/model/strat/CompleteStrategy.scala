package com.nicoburniske.model.strat

import com.nicoburniske.model.blackjack.{BlackjackModel, GameAction, Hit, Stand}


trait CompleteStrategy {
  def takeTurn(blackjack: BlackjackModel): Option[GameAction]
}

object HitSoftSeventeen extends CompleteStrategy {
  override def takeTurn(blackjack: BlackjackModel): Option[GameAction] = {
    val result = (blackjack.dealer.values.length, blackjack.dealer.bestValue) match {
      case (len, Some(17)) if len > 1 => Some(Hit) // hit on soft 17
      case (_, Some(best)) if best > 16 => Some(Stand)
      case (_, Some(best)) if best <= 16 => Some(Hit)
      case (_, None) => Some(Stand)
      case _ => None
    }
    result
  }
}

object StandSoftSeventeen extends CompleteStrategy {
  override def takeTurn(blackjack: BlackjackModel): Option[GameAction] = {
    blackjack.dealer.bestValue match {
      case Some(best) if best > 16 => Some(Stand)
      case Some(_) => Some(Hit)
      case _ => None
    }
  }
}

