package com.nicoburniske.view


import com.nicoburniske.model.blackjack.{Ace, BlackjackModel, Card, Club, Diamond, Eight, Five, Four, Hand, Heart, Jack, King, Nine, Queen, Rank, Seven, Six, Spade, Suit, Ten, Three, Two}


object TextView extends BlackjackView[String] {
  val suiteString: Map[Suit, String] = Map(Club -> "♣", Diamond -> "♦", Heart -> "♥", Spade ->"♠")
  val rankString: Map[Rank, String] = Map(Two ->"2", Three -> "3", Four -> "4", Five -> "5", Six -> "6", Seven -> "7",
  Eight -> "8", Nine -> "9", Ten -> "10", Jack -> "J", Queen -> "Q", King -> "K", Ace -> "A")
  val noCardString = "__"

  override def toView(state: BlackjackModel): String = {
    val builder = new StringBuilder()
    val dealerCards = state.dealerHand.cards.headOption match {
      case Some(value) => s"${this.cardString(value)} __"
      case None => new IllegalArgumentException("Dealer's hand cannot be empty")
    }
    builder.append(s"Dealer cards: $dealerCards\n")
    state.players.zipWithIndex.foreach { case (player, ind) =>
      val hand = this.handString(player.hand)
      builder.append(s"Player $ind cards: $hand\n")
    }
    builder.toString
  }

  def handString(hand: Hand): String = {
    s"${hand.cards.map(cardString).mkString(", ")} | ${hand.bestValue.getOrElse(hand.values.min)}"
  }

  def cardString(card: Card): String = {
    s"${rankString(card.rank)} ${suiteString(card.suit)}"
  }
}
