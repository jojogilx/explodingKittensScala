package players

import card._
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import players.Player._

object Player {
  type PlayerID = String
  type Hand = List[Card]

  implicit val playerEncoder: Encoder[Player]            = deriveEncoder[Player]
  implicit val playerDecoder: Decoder[Player]            = deriveDecoder[Player]
}

case class Player(playerID: PlayerID, seat: Int)

