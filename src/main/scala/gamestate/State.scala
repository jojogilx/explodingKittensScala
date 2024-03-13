package gamestate

import card.Deck
import players.Player
import players.Player._

case class State(drawDeck: Deck, discardDeck: Deck, currentPlayerIndex: Int, players: List[Player], playersHands: Map[PlayerID,Hand])
