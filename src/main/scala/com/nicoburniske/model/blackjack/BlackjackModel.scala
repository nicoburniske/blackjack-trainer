package com.nicoburniske.model.blackjack


case class GamePlayer(hand: Hand, score: Int, bet: Option[Int]) {
  def resetHand: GamePlayer = {
    GamePlayer(Hand(), score, bet)
  }

  def addToHand(card: Card): GamePlayer = {
    GamePlayer(this.hand.addCard(card), this.score, bet)
  }

  def isBroke: Boolean = this.score <= 0

  /**
   * Updates the players score based on the bet placed. Bet also gets reset
    * @param won -1 if lost, 0 if tied, 1 if won
   * @return a new player with updated attributes
   */
  def updateScore(won: Int): GamePlayer = {
    val betValue = this.bet.getOrElse(0)
    val newScore = won match {
      case 1 => score + 2 * betValue
      case -1 => score
      case 0 => score + betValue
      case _ => throw new IllegalArgumentException("Won must be between [-1, 1]")
    }
    GamePlayer(hand, newScore, None)
  }


  def dealFromDeck(deck: Deck, numCards: Int): (Deck, GamePlayer) = {
    val (newDeck, newHand) = this.hand.dealFromDeck(deck, numCards)
    (newDeck, GamePlayer(newHand, this.score, this.bet))
  }
}


class BlackjackModel(val dealer: Hand,
                     val players: List[GamePlayer],
                     val deck: Deck,
                     val deckCount: Int) {
  def playerAction(playerIndex: Int, action: GameAction): Option[BlackjackModel] = {
    action match {
      case Hit =>
        deck.dealCard match {
          case Some((card, newDeck)) =>
            val newPlayer = this.players(playerIndex).addToHand(card)
            val newPlayers = this.players.updated(playerIndex, newPlayer)
            Some(new BlackjackModel(this.dealer, newPlayers, newDeck, this.deckCount))
          case None =>
            // TODO: Ensure that this is supposed to happen
            this.resetDeck.playerAction(playerIndex, action)
        }
      case Stand =>
        Some(this)
      case Split =>
        None
      case Double =>
        val oldPlayer = this.players(playerIndex)
        oldPlayer.bet match {
          case Some(value) =>
            val newBet = value * 2
            val newScore = oldPlayer.score - value
            if (newScore > -1) {
              val newPlayer = GamePlayer(oldPlayer.hand, newScore, Some(newBet))
              val newPlayers = this.players.updated(playerIndex, newPlayer)
              val newState = new BlackjackModel(this.dealer, newPlayers, this.deck, this.deckCount)
              Some(newState.playerAction(playerIndex, Hit).get)
            } else {
              None
            }
          case None => None
        }

    }
  }

  def dealerAction(action: GameAction): Option[BlackjackModel] = {
    (action, this.deck.dealCard) match {
      case (Hit, Some((card, deck))) =>
        Some(new BlackjackModel(this.dealer.addCard(card), players, deck, this.deckCount))
      case (Stand, _) => Some(this)
      case (_, _) => None
    }
  }

  def resetHand: BlackjackModel = {
    new BlackjackModel(Hand(), players.map(_.resetHand), deck, this.deckCount)
  }

  def resetDeck: BlackjackModel = {
    new BlackjackModel(this.dealer, this.players, Deck(this.deckCount), deckCount)
  }

  def resetHandAndDeck: BlackjackModel = {
    new BlackjackModel(Hand(), players.map(_.resetHand), Deck(this.deckCount), deckCount)
  }

  def startHand: BlackjackModel = {
    if (this.deck.cards.length > Hand.MIN_CARDS * (this.players.length + 1)) {
      val newHand = this.resetHand
      val (newDeck, newPlayers) = newHand.players.foldLeft((newHand.deck, List.empty[GamePlayer])) { (acc, player) =>
        val afterAdd = player.dealFromDeck(acc._1, Hand.MIN_CARDS)
        (afterAdd._1, afterAdd._2 :: acc._2)
      }
      val (deckAfterDealer, dealerHand) = Hand().dealFromDeck(newDeck, Hand.MIN_CARDS)
      new BlackjackModel(dealerHand, newPlayers, deckAfterDealer, this.deckCount)
    } else {
      this.resetHandAndDeck.startHand
    }
  }

  def isOver: Boolean = {
    this.players.isEmpty ||
      !this.players.exists(_.score > 0)
  }

  def handOver: Boolean = {
    dealer.bestValue.isDefined && dealer.bestValue.get == Hand.WINNING_SCORE ||
      this.canPlay.isEmpty
  }

  def canPlay: List[Int] = {
    this.players.zipWithIndex
      .filter(tuple => tuple._1.hand.bestValue.isDefined)
      .map(_._2)
  }

  def removePlayer(index: Int): BlackjackModel = {
      val newPlayers = this.players.take(index) ++ this.players.drop(index + 1)
      this.updatePlayers(newPlayers)
  }

  def updatePlayers(newPlayers: Seq[GamePlayer]): BlackjackModel = {
    new BlackjackModel(this.dealer, newPlayers.toList, this.deck, this.deckCount)
  }
}
