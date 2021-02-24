package com.nicoburniske.blackjack_trainer.model.player

import com.nicoburniske.blackjack_trainer.model.game.{Double, GamePlayer, Hand, Hit, PlayerAction, Stand, Surrender}
import com.nicoburniske.blackjack_trainer.model.table.BlackjackHistory
import com.nicoburniske.blackjack_trainer.view.TextView

object PlayerCLI {
  val validInput: Map[String, PlayerAction] = Map("h" -> Hit, "s" -> Stand, "d" -> Double, "r" -> Surrender)
  val quitString: String = "q"
  val messageSectionLength = 100
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
         |${this.createOutputString("Welcome to Blackjack!")}
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

    println(createOutputString("Starting New Hand"))
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
         |${this.createOutputString("Hand Result")}
         |You $wonString!
         |Dealer's hand: ${TextView.handString(dealerHand)}
         |Your hand: ${TextView.handString(playerHand)}
         |""".stripMargin
    println(output)
    true
  }

  override def gameEnd(endScore: Int, history: BlackjackHistory): Boolean = {
    println(createOutputString("Game Over"))
    println(s"You left the casino with $endScore sheckles")
    println(s"You won ${history.winCount} times, You lost ${history.lossCount} times, You tied ${history.tieCount} times")
    val historyString = history.choices
      .map { case (model, action) => model.players(this.position.get).hand -> action }.toList.reverse.zipWithIndex
      .map { case (tuple, index) => s"Hand #${index + 1} ${TextView.handString(tuple._1)} - Decision ${tuple._2.getClass.getSimpleName}" }.mkString("\n")
    println(historyString)
    true
  }

  override def informOtherPlayerMove(info: PlayerView): Boolean = {
    println(playerViewString(info))
    true
  }

  private def playerViewString(info: PlayerView): String = {
    val builder = new StringBuilder()
    builder.append(s"Dealer card: ${TextView.handString(info.dealer)}\n")
    info.players.zipWithIndex.foreach { case (player, ind) =>
      val hand = TextView.handString(player.hand)
      builder.append(s"Player $ind cards: $hand\n")
    }
    builder.toString
  }

  override def getHandAction(player: GamePlayer, dealer: Hand): PlayerAction = {
    val prompt =
      s"""|${this.createOutputString("Current Table State")}
          |Dealer card: ${TextView.handString(dealer)}
          |Your hand: ${TextView.handString(player.hand)}
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

  private def createOutputString(message: String, totalLen: Int = PlayerCLI.messageSectionLength): String = {
    require(message.length <= totalLen)
    val remainingLen = totalLen - message.length
    val left: String = (0 until remainingLen / 2).map(_ => "-").mkString("")
    val right: String = (0 until (remainingLen - remainingLen / 2)).map(_ => "-").mkString("")
    s"$left $message $right"
  }

  //  implicit val showHand: Show[Hand] = (h: Hand) => {
  //    s"${h.cards.map().mkString(", ")} | ${h.bestValue.getOrElse(h.values.min)}"
  //  }

}
