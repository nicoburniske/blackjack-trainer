package com.nicoburniske.model.blackjack


/**
 * Represents a player in a game of Blackjack.
 *
 * @param hand  the current hand
 * @param score the players bankroll
 * @param bet   the current bet (if any)
 */
case class GamePlayer(hand: Hand, score: Int, bet: Option[Int]) {
  require(score >= 0)

  val isBroke: Boolean = this.score <= 0

  def resetHand: GamePlayer = GamePlayer(Hand(), score, bet)

  /**
   * Returns T/F whether the given bet is valid.
   *
   * @param potentialBet the bet to be considered
   * @return whether the given bet is valid
   */
  def validBet(potentialBet: Int): Boolean = {
    potentialBet > 0 && potentialBet <= score
  }

  /**
   * Returns a player with a doubled bet if possible.
   *
   * @return the new player if the current bet can be doubled, None otherwise
   */
  def doubleBet: Option[GamePlayer] = {
    this.bet match {
      case Some(value) =>
        if (this.score >= value)
          Some(GamePlayer(this.hand, this.score - value, Some(value * 2)))
        else
          None
      case None => None
    }
  }


  /**
   * Add the card to the player's hand.
   *
   * @param card the card to add
   * @return the new player with the updated hand
   */
  def addToHand(card: Card): GamePlayer = {
    GamePlayer(this.hand.addCard(card), this.score, bet)
  }

  /**
   * Updates the players score based on the bet placed. Bet also gets reset.
   *
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


  /**
   * Deals the specified number of cards to the current players hand.
   *
   * @param deck the deck to deal cards from
   * @param n    the number of cards to deal
   * @return (new Deck, new Player)
   */
  def dealFromDeck(deck: Deck, n: Int): (Deck, GamePlayer) = {
    val (newDeck, newHand) = this.hand.dealFromDeck(deck, n)
    (newDeck, GamePlayer(newHand, this.score, this.bet))
  }
}


/**
 * Represents the state of a game of Blackjack.
 *
 * @param dealerHand the dealer's current Hand
 * @param players    the players involved in the game
 * @param deck       the current deck
 * @param deckCount  the number of decks that are shuffled at once
 */
class BlackjackModel(val dealerHand: Hand,
                     val players: List[GamePlayer],
                     val deck: Deck,
                     val deckCount: Int) {
  /**
   * Perform the action for the player at the given index.
   *
   * @param playerIndex the index corresponding to the player to act
   * @param action      the action to perform
   * @return the updated game state if the action could be performed, None otherwise
   */
  def playerAction(playerIndex: Int, action: PlayerAction): Option[BlackjackModel] = {
    action match {
      case Hit =>
        deck.dealCard match {
          case Some((card, newDeck)) =>
            val newPlayer = this.players(playerIndex).addToHand(card)
            val newPlayers = this.players.updated(playerIndex, newPlayer)
            Some(new BlackjackModel(this.dealerHand, newPlayers, newDeck, this.deckCount))
          case None =>
            // TODO: Ensure that this is supposed to happen
            this.resetDeck.playerAction(playerIndex, action)
        }
      case Stand =>
        Some(this)
      case Split =>
        None
      case Double =>
        this.players(playerIndex).doubleBet match {
          case Some(newPlayer) =>
            val newPlayers = this.players.updated(playerIndex, newPlayer)
            val newState = new BlackjackModel(this.dealerHand, newPlayers, this.deck, this.deckCount)
            Some(newState.playerAction(playerIndex, Hit).get) // if double then the player must hit
          case None => None
        }
    }
  }

  /**
   * Perform the action for the dealer.
   *
   * @param action the action to perform
   * @return the updated game state i the action could be performed, None otherwise
   */
  def dealerAction(action: DealerAction): Option[BlackjackModel] = {
    (action, this.deck.dealCard) match {
      case (Hit, Some((card, deck))) =>
        Some(new BlackjackModel(this.dealerHand.addCard(card), players, deck, this.deckCount))
      case (Hit, None) => this.resetDeck.dealerAction(action) // TODO: should the deck be reset like this?
      case (Stand, _) => Some(this)
      case (_, _) => None
    }
  }

  /**
   * Resets the dealer's hand and each of the player's hands.
   *
   * @return the updated game state
   */
  def resetHand: BlackjackModel = {
    new BlackjackModel(Hand(), players.map(_.resetHand), deck, this.deckCount)
  }

  /**
   * Resets the deck.
   *
   * @return the updated game state
   */
  def resetDeck: BlackjackModel = {
    new BlackjackModel(this.dealerHand, this.players, Deck(this.deckCount), deckCount)
  }

  /**
   * Resets the dealer's hand and each of the player's hands. Also resets the deck to have n shuffled decks.
   *
   * @return the updated game state
   */
  def resetHandAndDeck: BlackjackModel = {
    new BlackjackModel(Hand(), players.map(_.resetHand), Deck(this.deckCount), deckCount)
  }

  /**
   * Starts the hand by dealing 2 cards to each player and 2 cards to the dealer.
   *
   * @return the updated game state
   */
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

  /**
   * Determines if any of the players can continue playing.
   *
   * @return T/F whether the players can continue to play
   */
  def isOver: Boolean = {
    this.players.isEmpty ||
      !this.players.exists(_.score > 0)
  }

  /**
   * Removes the player at the given index.
   *
   * @param index the player to remove
   * @return the updated game state
   */
  def removePlayer(index: Int): BlackjackModel = {
    val newPlayers = this.players.take(index) ++ this.players.drop(index + 1)
    this.updatePlayers(newPlayers)
  }

  /**
   * Updated the list of players to be the new list of players.
   *
   * @param newPlayers the new list of player
   * @return the updated game state
   */
  def updatePlayers(newPlayers: Seq[GamePlayer]): BlackjackModel = {
    new BlackjackModel(this.dealerHand, newPlayers.toList, this.deck, this.deckCount)
  }
}
