package com.nicoburniske.blackjack_trainer.model

import com.nicoburniske.blackjack_trainer.model.game.{Ace, Card, Hand, King, Spade, Two}
import org.scalatest.flatspec.AnyFlatSpec

class BlackjackModelTest extends AnyFlatSpec {
  val ace = Card(Spade, Ace)
  val king = Card(Spade, King)
  val two = Card(Spade, Two)
  val doubleAce = Hand(ace, ace)
  val twenty = Hand(king, king)
  val twentyTwo = Hand(king, king, two)
  val mtHand = Hand()

  behavior of "hand values"
  it should "obtain all possible hand values" in {
    assert(doubleAce.values == List(2, 12, 22))
    assert(twenty.values == List(20))
    assert(twentyTwo.values == List(22))
  }

  behavior of "bestValue"
  it should "obtain the best value for the player" in {
    assert(mtHand.bestValue.contains(0))
    assert(twenty.bestValue.contains(20))
    assert(twentyTwo.bestValue.isEmpty)
    assert(doubleAce.bestValue.contains(12))
  }
}
