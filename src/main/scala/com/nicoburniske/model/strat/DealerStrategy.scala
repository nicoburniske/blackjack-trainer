package com.nicoburniske.model.strat

import com.nicoburniske.model.blackjack.{BlackjackModel, DealerAction, Hit, Stand}


trait DealerStrategy {
  def takeTurn(blackjack: BlackjackModel): Option[DealerAction]
}

object HitSoftSeventeen extends DealerStrategy {
  override def takeTurn(blackjack: BlackjackModel): Option[DealerAction] = {
    val result = (blackjack.dealerHand.values.length, blackjack.dealerHand.bestValue) match {
      case (len, Some(17)) if len > 1 => Some(Hit) // hit on soft 17
      case (_, Some(best)) if best > 16 => Some(Stand)
      case (_, Some(best)) if best <= 16 => Some(Hit)
      case _ => None
    }
    result
  }
}

object StandSoftSeventeen extends DealerStrategy {
  override def takeTurn(blackjack: BlackjackModel): Option[DealerAction] = {
    blackjack.dealerHand.bestValue match {
      case Some(best) if best > 16 => Some(Stand)
      case Some(_) => Some(Hit)
      case _ => None
    }
  }
}

