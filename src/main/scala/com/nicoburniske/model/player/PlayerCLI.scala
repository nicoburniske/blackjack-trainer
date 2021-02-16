package com.nicoburniske.model.player

import com.nicoburniske.model.blackjack._
import com.nicoburniske.model.table.BlackjackHistory
import com.nicoburniske.view.TextView.handString

object PlayerCLI {
  val validInput: Map[String, PlayerAction] = Map("h" -> Hit, "s" -> Stand, "d" -> Double)
  val quitString: String = "q"
  val messageLen = 50
}

/**
 * Represents a player that can play Blackjack from the console.
 */
class PlayerCLI extends TablePlayer {

  // the position of the player at the table
  private var position: Option[Int] = None

  override def gameStart(): Boolean = {
    println(
      s"""
         |${this.createOutputString("Welcome to Blackjack!", PlayerCLI.messageLen)}
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

    println(createOutputString("Starting New Hand", PlayerCLI.messageLen))
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
    val output =
      s"""
        |${this.createOutputString("Hand Result", PlayerCLI.messageLen)}
        |You $wonString!
        |Dealer's hand: ${handString(dealerHand)}
        |Your hand: ${handString(playerHand)}
        |""".stripMargin
    println(output)
    true
  }

  override def gameEnd(endScore: Int, history: BlackjackHistory): Boolean = {
    println(createOutputString("Game Over", PlayerCLI.messageLen))
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
    val prompt =
      s"""|${this.createOutputString("Current Table State", PlayerCLI.messageLen)}
         |Dealer card: ${handString(dealer)}
         |Your hand: ${handString(player.hand)}
         |Please enter a move: Hit (h), Stand (s), Double (d)
         |""".stripMargin
    //TODO add more info
    val input = scala.io.StdIn.readLine(prompt)

    PlayerCLI.validInput.get(input.toLowerCase) match {
      case Some(value) => value
      case None =>
        println("Invalid action")
        getHandAction(player, dealer)
    }
  }

  private def createOutputString(message: String, totalLen: Int): String = {
    require(message.length <= totalLen)
    val remainingLen = totalLen - message.length
    val left: String = (0 until remainingLen / 2).map(_ => "-").mkString("")
    val right: String = (0 until (remainingLen - remainingLen / 2)).map(_ => "-").mkString("")
    s"$left $message $right"
  }
}
