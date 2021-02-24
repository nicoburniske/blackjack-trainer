package com.nicoburniske.blackjack_trainer.view

import com.nicoburniske.blackjack_trainer.model.game.BlackjackModel

trait BlackjackView[T] {
  def toView(state: BlackjackModel): T
}
