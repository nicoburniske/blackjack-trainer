package com.nicoburniske.blackjack_trainer.model.game

/**
 * Represents an action that can be executed by a player or dealer in a game of Blackjack
 */
sealed trait DealerAction
sealed trait PlayerAction
case object Hit extends PlayerAction with DealerAction
case object Stand extends PlayerAction with DealerAction
case object Split extends PlayerAction
case object Double extends PlayerAction
case object Surrender extends PlayerAction
