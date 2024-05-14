package gamestate

import card.{Card, Deck}
import cats.effect.IO
import cats.effect.kernel.Deferred
import players.Player
import players.Player._

case class State(
    started: Boolean,
    drawDeck: Deck,
    discardDeck: Deck,
    currentPlayerIndex: Int,
    turnsLeft: Int,
    players: List[Player],
    playersHands: Map[PlayerID, Hand],
    shownCards: Map[PlayerID, List[Card]],
    disconnections: Map[PlayerID, Deferred[IO, Boolean]],
    orderRight: Boolean,

)
