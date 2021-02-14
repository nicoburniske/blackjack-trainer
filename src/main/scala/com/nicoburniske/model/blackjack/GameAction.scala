package com.nicoburniske.model.blackjack

sealed trait GameAction
case object Hit extends GameAction
case object Stand extends GameAction
case object Split extends GameAction
case object Double extends GameAction
case object Quit extends GameAction