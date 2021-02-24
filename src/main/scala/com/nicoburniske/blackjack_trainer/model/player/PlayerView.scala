package com.nicoburniske.blackjack_trainer.model.player

import com.nicoburniske.blackjack_trainer.model.game.{GamePlayer, Hand}

case class PlayerView(dealer: Hand, players: List[GamePlayer])
