package com.nicoburniske.model.blackjack

import scala.util.Random

object Deck {
  /**
   * Creates a deck containing the cards of n complete decks.
   *
   * @param n the number of decks to include
   * @return the deck with n * 52 cards
   */
  def apply(n: Int): Deck = {
    new Deck((0 until n).flatMap(_ => Random.shuffle(COMPLETE_DECK)).toList)
  }

  /**
   * Var args constructor for a deck.
   *
   * @param cards the cards to be included in the deck
   * @return the new Deck
   */
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
