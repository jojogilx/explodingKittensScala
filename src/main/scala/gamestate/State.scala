package gamestate

import card.Deck
import cats.effect.IO
import cats.effect.kernel.Deferred
import players.Player
import players.Player._

case class State(drawDeck: Deck, discardDeck: Deck, currentPlayerIndex: Int, players: List[Player], playersHands: Map[PlayerID,Hand], disconnections: Map[PlayerID, Deferred[IO,Boolean]])
