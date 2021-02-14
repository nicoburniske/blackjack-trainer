package com.nicoburniske.model.player

import com.nicoburniske.model.blackjack._
import com.nicoburniske.view.TextView.handString

object PlayerCLI {
  val validInput: Map[String, GameAction] = Map("h" -> Hit, "s" -> Stand, "q" -> Quit, "d" -> Double)
}

class PlayerCLI extends Player {

  private var position: Option[Int] = None

  override def gameStart(id: Int): Boolean = {
    println(
      s"""Welcome to BlackJack game Player $id!
         |------------------------------------------
         |The game starts by dealing out two cards each to the player and dealer.
         |A score of 21 wins the game for the player or dealer. In the event of a tie, the dealer wins.
         |
         |You start with 100 coins
         |You can choose to bet any number of available coins at each hand. Once all the coins are exhausted, the game ends.
         |The user can choose to
         | - Hit by entering an "H"
         | - Stand by entering an "S"
         | - Double by entering a "D"
         | - Quit by entering a "Q"
         |Best of luck!
      """.stripMargin)
    this.position = Some(id)
    true
  }

  override def handStart(info: PlayerView): Int = {
    def getInput(bankroll: Int): Int = {
      println(s"Please enter your starting bet (Min 1). Current bankroll: $bankroll.")
      scala.io.StdIn.readInt()
    }

    println("---------------- Starting New Hand ----------------")
    val bankroll = info.players(this.position.get).score
    val bet = getInput(bankroll)
    if (bet > 0 && bet <= bankroll) {
      bet
    } else {
      println("Bet must be greater than 1, and less than your bankroll")
      handStart(info)
    }
  }


  override def informHandResult(won: Int, playerHand: Hand, dealerHand: Hand): Boolean = {
    val wonString = {
      won match {
        case 1 => "won"
        case 0 => "tied"
        case -1 => "lost"
        case _ => throw new IllegalArgumentException("Won must be between [-1,1]")
      }
    }
    val output = {
      s"-------------- Hand Result ------------\n" +
        s"You $wonString!\n" +
        s"Your hand ${handString(playerHand)}\n" +
        s"Dealer's hand ${handString(dealerHand)}"
    }
    println(output)
    true
  }


  override def gameEnd(endScore: Int): Boolean = {
    println("---------------   Game Over  ----------------------")
    println(s"You left the casino with $endScore sheckles")
    true
  }

  override def informOtherPlayerMove(info: PlayerView): Boolean = {
    println(playerViewString(info))
    true
  }

  private def playerViewString(info: PlayerView): String = {
    val builder = new StringBuilder()
    builder.append(s"Dealer card: ${handString(info.dealer)}\n")
    info.players.zipWithIndex.foreach { case (player, ind) =>
      val hand = handString(player.hand)
      builder.append(s"Player $ind cards: $hand\n")
    }
    builder.toString
  }

  override def getHandAction(player: GamePlayer, dealer: Hand): GameAction = {
    val prompt = "----------Current table state------------\n" +
      s"Dealer card: ${handString(dealer)}\n" +
      s"Player cards: ${handString(player.hand)}\n" +
      s"Please enter a move: Hit (h), Stand (s), Quit (q), Double (d)\n"
    //TODO add more info
    val input = scala.io.StdIn.readLine(prompt)

    PlayerCLI.validInput.get(input.toLowerCase) match {
      case Some(value) => value
      case None =>
        println("Invalid action")
        getHandAction(player, dealer)
    }
  }


}
