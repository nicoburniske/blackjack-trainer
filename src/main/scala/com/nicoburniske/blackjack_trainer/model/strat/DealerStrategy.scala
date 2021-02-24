package com.nicoburniske.blackjack_trainer.model.strat

import com.nicoburniske.blackjack_trainer.model.game.{DealerAction, Hand, Hit, Stand}


trait DealerStrategy {
  def takeTurn(hand: Hand): Option[DealerAction]
}

object HitSoftSeventeen extends DealerStrategy {
  override def takeTurn(hand: Hand): Option[DealerAction] = {
    val result = (hand.values.length, hand.bestValue) match {
      case (len, Some(17)) if len > 1 => Some(Hit) // hit on soft 17
      case (_, Some(best)) if best > 16 => Some(Stand)
      case (_, Some(best)) if best <= 16 => Some(Hit)
      case _ => None
    }
    result
  }
}

object StandSoftSeventeen extends DealerStrategy {
  override def takeTurn(hand: Hand): Option[DealerAction] = {
    hand.bestValue match {
      case Some(best) if best > 16 => Some(Stand)
      case Some(_) => Some(Hit)
      case _ => None
    }
  }
}

