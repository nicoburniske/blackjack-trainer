package com.nicoburniske.blackjack_trainer.view

import cats.Show
import cats.syntax.show._
import com.nicoburniske.blackjack_trainer.model.game._
import com.nicoburniske.blackjack_trainer.model.player.PlayerView // for show


object TextView extends BlackjackView[String] {

  override def toView(state: BlackjackModel): String = {
    val builder = new StringBuilder()
    val dealerCards = state.dealerHand.cards.headOption match {
      case Some(value) => s"${value.show} __"
      case None => new IllegalArgumentException("Dealer's hand cannot be empty")
    }
    builder.append(s"Dealer cards: $dealerCards\n")
    state.players.zipWithIndex.foreach { case (player, ind) =>
      val hand = player.hand.show
      builder.append(s"Player $ind cards: $hand\n")
    }
    builder.toString
  }

  implicit val showSuite: Show[Suit] = {
    case Club => "♣"
    case Diamond => "♦"
    case Heart => "♥"
    case Spade => "♠"
  }

  implicit val showRank: Show[Rank] = {
    case Ace => "A"
    case King => "K"
    case Queen => "Q"
    case Jack => "J"
    case Ten => "10"
    case Nine => "9"
    case Eight => "8"
    case Seven => "7"
    case Six => "6"
    case Five => "5"
    case Four => "4"
    case Three => "3"
    case Two => "2"
  }

  implicit val showCard: Show[Card] = (c: Card) => {
    s"${c.rank.show} ${c.suit.show}"
  }

  implicit val showHand: Show[Hand] = (h: Hand) => {
    s"${h.cards.map(_.show).mkString(", ")} | ${h.bestValue.getOrElse(h.values.min)}"
  }

  implicit val showAction: Show[PlayerAction] = {
    case Hit => "Hit"
    case Stand => "Stand"
    case Split => "Split"
    case Double => "Double"
    case Surrender => "Surrender"
  }

  implicit val showPlayerView: Show[PlayerView] = (info: PlayerView) => {
    val playerHands = info.players.zipWithIndex.map { case (player, ind) =>
      s"Player $ind cards: ${player.hand.show}"
    }.mkString("\n")
    s"""Dealer's card: ${info.dealer.show}
       |$playerHands
       |""".stripMargin
  }
}
