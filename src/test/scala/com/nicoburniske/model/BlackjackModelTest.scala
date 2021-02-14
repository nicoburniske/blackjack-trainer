//package scala.com.nicoburniske.model
//
//import scala.com.nicoburniske.model.blackjack.{Ace, Card, Hand, King, Spade, Two}
//
//class BlackjackModelTest extends AnyFlatSpec {
//  val ace = Card(Spade, Ace)
//  val king = Card(Spade, King)
//  val two = Card(Spade, Two)
//  val doubleAce = Hand(List(ace, ace))
//  val twenty = Hand(List(king, king))
//  val twentyTwo = Hand(List(king, king, two))
//  val mtHand = Hand()
//
//  behavior of "hand values"
//  it should "obtain all possible hand values" in {
//    assert(doubleAce.values == List(2, 12, 22))
//    assert(twenty.values == List(20))
//    assert(twentyTwo.values == List(22))
//  }
//
//  behavior of "bestValue"
//  it should "obtain the best value for the player" in {
//    assert(mtHand.bestValue.contains(0))
//    assert(twenty.bestValue.contains(20))
//    assert(twentyTwo.bestValue.isEmpty)
//    assert(doubleAce.bestValue.contains(12))
//  }
//
//
//}
//package scala.com.nicoburniske.model
//
//import scala.com.nicoburniske.model.blackjack.{Ace, Card, Hand, King, Spade, Two}
//
//class BlackjackModelTest extends AnyFlatSpec {
//  val ace = Card(Spade, Ace)
//  val king = Card(Spade, King)
//  val two = Card(Spade, Two)
//  val doubleAce = Hand(List(ace, ace))
//  val twenty = Hand(List(king, king))
//  val twentyTwo = Hand(List(king, king, two))
//  val mtHand = Hand()
//
//  behavior of "hand values"
//  it should "obtain all possible hand values" in {
//    assert(doubleAce.values == List(2, 12, 22))
//    assert(twenty.values == List(20))
//    assert(twentyTwo.values == List(22))
//  }
//
//  behavior of "bestValue"
//  it should "obtain the best value for the player" in {
//    assert(mtHand.bestValue.contains(0))
//    assert(twenty.bestValue.contains(20))
//    assert(twentyTwo.bestValue.isEmpty)
//    assert(doubleAce.bestValue.contains(12))
//  }
//
//
//}
