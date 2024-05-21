package gamestate

import card.Deck
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
    playersHands: Map[PlayerID, (Hand,HandCount)],
    disconnections: Map[PlayerID, Deferred[IO, Boolean]],
    marking: List[(PlayerID,PlayerID)],
    orderRight: Boolean,
    passed: Boolean,
    skipped: Boolean,
    barkingKitten: Option[PlayerID]
)
