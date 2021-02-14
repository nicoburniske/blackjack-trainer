package com.nicoburniske.model.blackjack

object Hand {
  val WINNING_SCORE = 21
  val MIN_CARDS = 2

  def apply(): Hand = {
    Hand(Nil)
  }
  def apply(cards: Card*) : Hand= {
    Hand(cards.toList)
  }
}

/**
 * Represents a Hand of cards
 * Used for both the player and dealer
 */
case class Hand(cards: List[Card]) {

  /**
   * Creates a new hand with this card added to it
   */
  def addCard(c: Card): Hand = Hand(c :: this.cards)

  /**
   * Computes all possible values of the hand.
   */
  def values: List[Int] = {
    this.cards.foldLeft(List(0)) { (acc, card) =>
      for {
        possibleSum <- acc
        cardValue <- card.rank.value
      } yield possibleSum + cardValue
    }.distinct
  }

  /**
   * Calculates the hand's best value.
   *
   * @return
   *  - None if the hand is bust (over MAX_VALUE)
   *  - Some(the best value)
   */
  def bestValue: Option[Int] = {
    this.values
      .filter(_ <= Hand.WINNING_SCORE)
      .reduceOption(_ max _)
  }

  def winsOver(dealer: Hand): Option[Boolean] = {
    (this.bestValue, dealer.bestValue) match {
      case (_, Some(Hand.WINNING_SCORE)) => Some(false)
      case (Some(best), Some(dealerBest)) => Some(best > dealerBest)
      case (Some(_), None) => Some(true)
      case (None, Some(_)) => Some(false)
      case (_, _) => None
    }
  }

  def canHit: Boolean = {
    this.values.exists(_ < Hand.WINNING_SCORE)
  }

  def dealFromDeck(deck: Deck, numCards: Int): (Deck, Hand) = {
    (0 until numCards).foldLeft((deck, this)) { (acc, _) =>
      acc._1.dealCard match {
        case Some((card, newDeck)) => (newDeck, acc._2.addCard(card))
        case None => acc
      }
    }
  }

  def hideAllButFirst: Hand = {
    if (this.cards.isEmpty) {
      this
    } else {
      Hand(this.cards.head)
    }
  }
}