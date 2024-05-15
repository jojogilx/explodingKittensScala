package players

import card._
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import players.Player._

object Player {
  type PlayerID = String
  type Hand = List[Card]
  case class HandCount(hidden: Int, shown: List[Card])

  implicit val playerEncoder: Encoder[Player]            = deriveEncoder[Player]
  implicit val playerDecoder: Decoder[Player]            = deriveDecoder[Player]

  implicit val handEncoder: Encoder[HandCount]            = deriveEncoder[HandCount]
  implicit val handDecoder: Decoder[HandCount]            = deriveDecoder[HandCount]
}

case class Player(playerID: PlayerID, seat: Int)

