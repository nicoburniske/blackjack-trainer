package com.nicoburniske.main

import com.nicoburniske.model.BlackjackRef
import com.nicoburniske.model.player.PlayerCLI
import com.nicoburniske.model.strat.HitSoftSeventeen


object Main {
  def main(args: Array[String]): Unit = {
    BlackjackRef.playGame(List(new PlayerCLI()), HitSoftSeventeen)
  }
}
