package com.nicoburniske.model.blackjack

/**
 * Represents a Card suit - Hearts, Diamonds, Spades, Clubs
 */
sealed trait Suit
case object Heart extends Suit
case object Diamond extends Suit
case object Spade extends Suit
case object Club extends Suit


/**
 * Represents a Card rank - Ace, King, Queen, Jack, 10 - 1
 */
sealed abstract class Rank(val value: List[Int])
case object King extends Rank(List(10))
case object Queen extends Rank(List(10))
case object Jack extends Rank(List(10))
case object Ten extends Rank(List(10))
case object Nine extends Rank(List(9))
case object Eight extends Rank(List(8))
case object Seven extends Rank(List(7))
case object Six extends Rank(List(6))
case object Five extends Rank(List(5))
case object Four extends Rank(List(4))
case object Three extends Rank(List(3))
case object Two extends Rank(List(2))
case object Ace extends Rank(List(1, 11))

/**
 * Represents a card
 */
case class Card(suit: Suit, rank: Rank)
