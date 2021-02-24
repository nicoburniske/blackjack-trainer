package com.nicoburniske.blackjack_trainer.model.player

import cats.syntax.show._
import com.nicoburniske.blackjack_trainer.model.game.{Double, GamePlayer, Hand, Hit, PlayerAction, Stand, Surrender}
import com.nicoburniske.blackjack_trainer.model.table.BlackjackHistory
import com.nicoburniske.blackjack_trainer.view.TextView._

object PlayerCLI {
  val validInput: Map[String, PlayerAction] = Map("h" -> Hit, "s" -> Stand, "d" -> Double, "r" -> Surrender)
  val quitString: String = "q"
  val messageSectionLength = 100

  private def createOutputString(message: String, totalLen: Int = PlayerCLI.messageSectionLength): String = {
    require(message.length <= totalLen, "Message cannot be longer than total length")
    val remainingLen = totalLen - message.length
    val left: String = (0 until remainingLen / 2).map(_ => "-").mkString("")
    val right: String = (0 until (remainingLen - remainingLen / 2)).map(_ => "-").mkString("")
    s"$left $message $right"
  }
}

/**
 * Represents a player that can play Blackjack from the console.
 */
class PlayerCLI() extends TablePlayer {

  // the position of the player at the table
  private var position: Option[Int] = None

  override def gameStart(): Boolean = {
    println(
      s"""
         |${PlayerCLI.createOutputString("Welcome to Blackjack!")}
         |The game starts by dealing out two cards each to each player and dealer.
         |A score of 21 wins the game for the player or dealer
         |
         |You start with 100 coins
         |You can choose to bet any number of available coins at each hand. Once all the coins are exhausted, the game ends.
         |The user can choose to
         | - Hit by entering "H"
         | - Stand by entering "S"
         | - Double by entering "D"
         | - Quit by entering "Q"
         | - Surrender by entering "R"
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

    println(PlayerCLI.createOutputString("Starting New Hand"))
    val response = getInput(bankroll)
    response.toIntOption match {
      case Some(bet) =>
        if (bet > 0 && bet <= bankroll) {
          Some(bet)
        } else {
          println("Bet must be greater than 1 and less than your bankroll")
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
         |${PlayerCLI.createOutputString(s"Hand Result: You $wonString!")}
         |Dealer's hand: ${dealerHand.show}
         |Your hand: ${playerHand.show}
         |""".stripMargin
    println(output)
    true
  }

  override def gameEnd(endScore: Int, history: BlackjackHistory): Boolean = {
    println(PlayerCLI.createOutputString("Game Over"))
    println(s"You left the casino with $endScore sheckles")
    println(s"You won ${history.winCount} times, You lost ${history.lossCount} times, You tied ${history.tieCount} times")
    val historyString = history.choices
      .map { case (model, action) => model.players(this.position.get).hand -> action }.toList.reverse.zipWithIndex
      .map { case (tuple, index) => s"Hand #${index + 1} ${tuple._1.show} - Decision ${tuple._2.show}" }.mkString("\n")
    println(historyString)
    true
  }

  override def informOtherPlayerMove(info: PlayerView): Boolean = {
    println(info.show)
    true
  }

  override def getHandAction(player: GamePlayer, dealer: Hand): PlayerAction = {
    val prompt =
      s"""|${PlayerCLI.createOutputString("It's your turn!")}
          |Dealer's hand: ${dealer.show}
          |Your hand: ${player.hand.show}
          |Please enter a move: Hit (h), Stand (s), Double (d), or Surrender (r)
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
}
