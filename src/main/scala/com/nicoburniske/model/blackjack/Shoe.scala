package com.nicoburniske.model.blackjack

import scala.util.Random

object Shoe {
  /**
   * Creates a Shoe containing the cards of n complete decks.
   *
   * @param n the number of decks to include
   * @return the Shoe with n * 52 cards
   */
  def apply(n: Int): Shoe = {
    new Shoe(Random.shuffle((0 until n).flatMap(_ => COMPLETE_DECK).toList))
  }

  /**
   * Var args constructor for a Shoe
   *
   * @param cards the cards to be included in the Shoe
   * @return the new Shoe
   */
  def apply(cards: Card*): Shoe = {
    new Shoe(cards.toList)
  }

  /**
   * A complete deck of 52 cards
   */
  val COMPLETE_DECK: List[Card] =
    for {
      suit <- List(Heart, Diamond, Spade, Club)
      rank <- List(King, Queen, Jack, Ten, Nine, Eight, Seven, Six, Five, Four, Three, Two, Ace)
    } yield Card(suit, rank)
}

/**
 * Represents a dealing shoe to hold 1 or more decks of playing cards
 */
class Shoe(val cards: List[Card]) {
  /**
   * Method that deals a card from the shoe.
   * The top card is removed from the shoe and returned along with the new shoe
   */
  def dealCard: Option[(Card, Shoe)] = {
    if (this.cards.isEmpty)
      None
    else
      Some(this.cards.head, new Shoe(this.cards.tail))
  }
}
