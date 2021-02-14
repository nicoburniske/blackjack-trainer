package com.nicoburniske.model.blackjack

import scala.util.Random

object Deck {
  def apply(numDecks: Int): Deck = {
    val cards: List[Card] = (0 until numDecks).flatMap(_ => Random.shuffle(COMPLETE_DECK)).toList
    new Deck(cards)
  }

  def apply(cards: Card*): Deck = {
    new Deck(cards.toList)
  }

  /**
   * Method that initializes the deck of cards
   */
  val COMPLETE_DECK: List[Card] =
    for {
      suit <- List(Heart, Diamond, Spade, Club)
      rank <- List(King, Queen, Jack, Ten, Nine, Eight, Seven, Six, Five, Four, Three, Two, Ace)
    } yield Card(suit, rank)
}

/**
 * Represents a deck of cards
 */
class Deck(val cards: List[Card]) {
  /**
   * Method that deals a card from the deck.
   * The top card is removed from the deck and returned
   */
  def dealCard: Option[(Card, Deck)] = {
    if (this.cards.isEmpty)
      None
    else
      Some(this.cards.head, new Deck(this.cards.tail))
  }
}
