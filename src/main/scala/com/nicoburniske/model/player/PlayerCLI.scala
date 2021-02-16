package com.nicoburniske.model.player

import com.nicoburniske.model.blackjack._
import com.nicoburniske.model.ref.BlackjackHistory
import com.nicoburniske.view.TextView.handString

object PlayerCLI {
  val validInput: Map[String, PlayerAction] = Map("h" -> Hit, "s" -> Stand, "d" -> Double)
  val quitString: String = "q"
}

/**
 * Represents a player that can play Blackjack from the console.
 */
class PlayerCLI extends TablePlayer {

  // the position of the player at the table
  private var position: Option[Int] = None

  override def gameStart(): Boolean = {
    println(
      s"""Welcome to BlackJack!
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
    true
  }

  override def setPlayerPosition(position: Int): Boolean = {
    this.position = Some(position)
    true
  }

  override def handStart(bankroll: Int): Option[Int] = {
    def getInput(bankroll: Int): String = {
      scala.io.StdIn.readLine(s"Please enter your starting bet (min 1) or quit (q). Current bankroll: $bankroll.\n")
    }

    println("---------------- Starting New Hand ----------------")
    val response = getInput(bankroll)
    response.toIntOption match {
      case Some(bet) =>
        if (bet > 0 && bet <= bankroll) {
          Some(bet)
        } else {
          println("Bet must be greater than 1, and less than your bankroll")
          handStart(bankroll)
        }
      case None =>
        if (response.toLowerCase == PlayerCLI.quitString) {
          None
        } else {
          println("Invalid input")
          handStart(bankroll)
        }
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
        s"Dealer's hand: ${handString(dealerHand)}\n" +
        s"Your hand: ${handString(playerHand)}"
    }
    println(output)
    true
  }

  override def gameEnd(endScore: Int, history: BlackjackHistory): Boolean = {
    println("---------------   Game Over  ----------------------")
    println(s"You left the casino with $endScore sheckles")
    println(s"You won ${history.winCount} times, You lost ${history.lossCount} times, You tied ${history.tieCount} times")
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

  override def getHandAction(player: GamePlayer, dealer: Hand): PlayerAction = {
    val prompt = "----------Current table state------------\n" +
      s"Dealer card: ${handString(dealer)}\n" +
      s"Your hand: ${handString(player.hand)}\n" +
      s"Please enter a move: Hit (h), Stand (s), Double (d)\n"
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
