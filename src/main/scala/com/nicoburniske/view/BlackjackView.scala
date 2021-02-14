package com.nicoburniske.view

import com.nicoburniske.model.blackjack.BlackjackModel

trait BlackjackView[T] {
  def toView(state: BlackjackModel): T
}
