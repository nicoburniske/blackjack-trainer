package com.nicoburniske.main

import com.nicoburniske.model.player.PlayerCLI
import com.nicoburniske.model.table.Blackjack
import com.nicoburniske.model.strat.HitSoftSeventeen


object Main {
  def main(args: Array[String]): Unit = {
    Blackjack.playGame(List(new PlayerCLI()), HitSoftSeventeen)
  }
}
