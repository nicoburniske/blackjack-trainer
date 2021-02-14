package com.nicoburniske.model.player

import com.nicoburniske.model.blackjack.{GamePlayer, Hand}

case class PlayerView(dealer: Hand, players: List[GamePlayer])
