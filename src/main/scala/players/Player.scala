package players

import card._
import players.Player._

object Player {
  type PlayerID = String
  type Hand = List[Card]
}

case class Player(playerID: PlayerID)

