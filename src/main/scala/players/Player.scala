package players

import card._
import cats.effect.IO
import cats.effect.std.Queue
import cats.implicits.catsSyntaxOptionId
import players.Player._

object Player {
  type PlayerID = String
  type Hand = List[Card]
}

case class Player(playerID: PlayerID, color: String)

