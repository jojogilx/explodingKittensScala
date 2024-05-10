package websockethub

import card.Recipes._
import card.{Card, Recipe}
import io.circe.Encoder
import io.circe.generic.semiauto.deriveEncoder
import io.circe.syntax.EncoderOps
import players.Player.{Hand, PlayerID}
sealed trait Event
object Event {


  case class Joined(player: String, player_list: List[(String, Int)]) extends Event
  case class LeftGame(player: String, player_list: List[(String, Int)]) extends Event
  case class Started()                                                        extends Event
  case class Information(information: String)                                 extends Event
  case class Error(error: String)                                 extends Event
  case class RoomStateEvent(player_list: List[(String, Int)], recipe: Recipe) extends Event
  case class NewTurn(player: String) extends Event
  case class Winner(player: String) extends Event
  case class HandEvent(player_hand: Hand) extends Event
  case class PilesUpdate(draw_size: Int, last_discarded: Option[String]) extends Event
  case class DrawCardEvent(card: Card, playerID: Option[PlayerID]) extends Event
  case class PlayCardEvent(card: Card) extends Event



  implicit val joinedEncoder: Encoder[Joined]            = deriveEncoder[Joined]
  implicit val leftGameEncoder: Encoder[LeftGame]        = deriveEncoder[LeftGame]
  implicit val startedEncoder: Encoder[Started]          = deriveEncoder[Started]
  implicit val informationEncoder: Encoder[Information]  = deriveEncoder[Information]
  implicit val errorEncoder: Encoder[Error]  = deriveEncoder[Error]
  implicit val newturnEncoder: Encoder[NewTurn]  = deriveEncoder[NewTurn]
  implicit val winnerEncoder: Encoder[Winner]  = deriveEncoder[Winner]
  implicit val roomStateEncoder: Encoder[RoomStateEvent] = deriveEncoder[RoomStateEvent]
  implicit val handEncoder: Encoder[HandEvent] = deriveEncoder[HandEvent]
  implicit val pilesEncoder: Encoder[PilesUpdate] = deriveEncoder[PilesUpdate]
  implicit val cardEncoder: Encoder[DrawCardEvent] = deriveEncoder[DrawCardEvent]
  implicit val pcardEncoder: Encoder[PlayCardEvent] = deriveEncoder[PlayCardEvent]



  implicit val eventEncoder: Encoder[Event] = Encoder.instance {
    case joined: Joined            => joined.asJson.mapObject(_.add("event", "joined".asJson))
    case left: LeftGame            => left.asJson.mapObject(_.add("event", "left".asJson))
    case started: Started          => started.asJson.mapObject(_.add("event", "started".asJson))
    case information: Information  => information.asJson.mapObject(_.add("event", "information".asJson))
    case information: Error  => information.asJson.mapObject(_.add("event", "error".asJson))
    case information: NewTurn  => information.asJson.mapObject(_.add("event", "new_turn".asJson))
    case information: Winner  => information.asJson.mapObject(_.add("event", "winner".asJson))
    case roomState: RoomStateEvent => roomState.asJson.mapObject(_.add("event", "room_state".asJson))
    case hand: HandEvent => hand.asJson.mapObject(_.add("event", "hand".asJson))
    case piles: PilesUpdate => piles.asJson.mapObject(_.add("event", "piles".asJson))
    case card: DrawCardEvent => card.asJson.mapObject(_.add("event", "draw_card".asJson))
    case card: DrawCardEvent => card.asJson.mapObject(_.add("event", "play_card".asJson))
  }
}
