package com.nicoburniske.blackjack_trainer.model.game

object Hand {
  val WINNING_SCORE = 21
  val MIN_CARDS = 2

  /**
   * Alternate constructor for an empty hand.
   */
  def apply(): Hand = {
    Hand(Nil)
  }

  /**
   * Alternate var-args constructor for a non-empty hand.
   */
  def apply(cards: Card*): Hand = {
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
   * Returns a List containing 0 if there are no cards in the hand
   */
  val values: List[Int] = {
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


  /**
   * Finds the stronger hand.
   *
   * @param dealer the other hand
   * @return
   * 1 - this hand is stronger
   * 0 - the hands are tied
   * -1 - the dealer's hand is stronger
   */
  def winsOver(dealer: Hand): Option[Int] = {
    (this.bestValue, dealer.bestValue) match {
      case (Some(best), Some(dealerBest)) => Some(best.compareTo(dealerBest))
      case (Some(_), None) => Some(1)
      case (None, Some(_)) => Some(-1)
      case (_, _) =>
        println("ERROR" + dealer.toString + this.toString)
        None
    }
  }

  /**
   * Determines if the hand can be dealt another card.
   */
  def canHit: Boolean = {
    !this.bestValue.contains(Hand.WINNING_SCORE) && this.values.exists(_ < Hand.WINNING_SCORE)
  }

  /**
   * Deals the specified number of cards to the current hand.
   *
   * @param shoe the shoe to deal cards from
   * @param n    the number of cards to deal
   * @return (new Shoe, new Hand)
   */
  def dealFromShoe(shoe: Shoe, n: Int): (Shoe, Hand) = {
    (0 until n).foldLeft((shoe, this)) { (acc, _) =>
      acc._1.dealCard match {
        case Some((card, newShoe)) => (newShoe, acc._2.addCard(card))
        case None => acc
      }
    }
  }

  /**
   * Creates a new Hand only containing the first card of the current hand.
   *
   * @return a new Hand
   */
  def hideAllButFirst: Hand = {
    if (this.cards.isEmpty) {
      this
    } else {
      Hand(this.cards.head)
    }
  }
}