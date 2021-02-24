package com.nicoburniske.blackjack_trainer.main

import com.nicoburniske.blackjack_trainer.model.player.PlayerCLI
import com.nicoburniske.blackjack_trainer.model.strat.HitSoftSeventeen
import com.nicoburniske.blackjack_trainer.model.table.Blackjack


object Main {
  def main(args: Array[String]): Unit = {
    Blackjack.playGame(List(new PlayerCLI()), HitSoftSeventeen)
    ()
  }
}
