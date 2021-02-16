package com.nicoburniske.main

import com.nicoburniske.model.player.PlayerCLI
import com.nicoburniske.model.ref.BlackjackTable
import com.nicoburniske.model.strat.HitSoftSeventeen


object Main {
  def main(args: Array[String]): Unit = {
    BlackjackTable.playGame(List(new PlayerCLI()), HitSoftSeventeen)
  }
}
