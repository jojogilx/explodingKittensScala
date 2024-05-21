package websockethub

import card.Recipes._
import card._
import io.circe.Encoder
import io.circe.generic.semiauto.deriveEncoder
import io.circe.syntax.EncoderOps
import players.Player
import players.Player._
sealed trait Event
object Event {

  // game logistics related
  case class Joined(player: String, player_list: List[Player])         extends Event
  case class LeftGame(player: String, player_list: List[Player])       extends Event
  case class Started()                                                 extends Event
  case class RoomStateEvent(player_list: List[Player], recipe: Recipe) extends Event

  // general
  case class Information(information: String) extends Event
  case class Error(error: String)             extends Event

  // turns, win & lose
  case class NewTurn(player: String) extends Event
  case class Winner(player: String)  extends Event
  case class Died(player: String)    extends Event

  // State Updates
  case class CardsInHand(player_hand: Hand)                              extends Event
  case class PilesUpdate(draw_size: Int) extends Event
  case class PlayersHands(hands: List[(PlayerID, HandCount)])            extends Event

  // card operations
  case class DrawCard(card: Card, playerID: Option[PlayerID]) extends Event
  case class PlayCard(card: Card, playerID: Option[PlayerID]) extends Event

  // card specific
  case class TargetPlayer(players: List[PlayerID])            extends Event
  case class BuryCard(card: Option[Card], min: Int, max: Int) extends Event
  case class ChooseCard(cards: List[Card])                    extends Event
  case class GarbageCollect()                                 extends Event
  case class AlterCardOrder(next_cards: List[Card])           extends Event
  case class SeeCards(cards: List[Card])                 extends Event

  case class ActionNoped() extends Event
  case class GetNopes(cards: List[Card]) extends Event
  case class Timer(seconds: Int) extends Event


  // encoders
  implicit val joinedEncoder: Encoder[Joined]            = deriveEncoder[Joined]
  implicit val leftGameEncoder: Encoder[LeftGame]        = deriveEncoder[LeftGame]
  implicit val startedEncoder: Encoder[Started]          = deriveEncoder[Started]
  implicit val roomStateEncoder: Encoder[RoomStateEvent] = deriveEncoder[RoomStateEvent]

  implicit val informationEncoder: Encoder[Information] = deriveEncoder[Information]
  implicit val errorEncoder: Encoder[Error]             = deriveEncoder[Error]

  implicit val newturnEncoder: Encoder[NewTurn] = deriveEncoder[NewTurn]
  implicit val winnerEncoder: Encoder[Winner]   = deriveEncoder[Winner]
  implicit val diedEncoder: Encoder[Died]       = deriveEncoder[Died]

  implicit val handEncoder: Encoder[CardsInHand]          = deriveEncoder[CardsInHand]
  implicit val pilesEncoder: Encoder[PilesUpdate]         = deriveEncoder[PilesUpdate]
  implicit val playersHandsEncoder: Encoder[PlayersHands] = deriveEncoder[PlayersHands]

  implicit val drawEncoder: Encoder[DrawCard] = deriveEncoder[DrawCard]
  implicit val playEncoder: Encoder[PlayCard] = deriveEncoder[PlayCard]

  implicit val targetEncoder: Encoder[TargetPlayer] = deriveEncoder[TargetPlayer]
  implicit val buryEncoder: Encoder[BuryCard]       = deriveEncoder[BuryCard]
  implicit val chooseCardEncoder: Encoder[ChooseCard]       = deriveEncoder[ChooseCard]
  implicit val garbageCollectEncoder: Encoder[GarbageCollect]       = deriveEncoder[GarbageCollect]
  implicit val alterOrderEncoder: Encoder[AlterCardOrder]       = deriveEncoder[AlterCardOrder]
  implicit val seeCardsEncoder: Encoder[SeeCards]       = deriveEncoder[SeeCards]


  implicit val nopedEncoder: Encoder[ActionNoped]       = deriveEncoder[ActionNoped]
  implicit val getNopesEncoder: Encoder[GetNopes]       = deriveEncoder[GetNopes]
  implicit val timerEncoder: Encoder[Timer]       = deriveEncoder[Timer]

  implicit val eventEncoder: Encoder[Event] = Encoder.instance {
    case joined: Joined            => joined.asJson.mapObject(_.add("event", "joined".asJson))
    case left: LeftGame            => left.asJson.mapObject(_.add("event", "left".asJson))
    case started: Started          => started.asJson.mapObject(_.add("event", "started".asJson))
    case roomState: RoomStateEvent => roomState.asJson.mapObject(_.add("event", "room_state".asJson))
    case information: Information  => information.asJson.mapObject(_.add("event", "information".asJson))
    case error: Error        => error.asJson.mapObject(_.add("event", "error".asJson))
    case newTurn: NewTurn      =>   newTurn.asJson.mapObject(_.add("event", "new_turn".asJson))
    case winner: Winner       => winner.asJson.mapObject(_.add("event", "winner".asJson))
    case died: Died       => died.asJson.mapObject(_.add("event", "died".asJson))
    case hand: CardsInHand         => hand.asJson.mapObject(_.add("event", "hand".asJson))
    case piles: PilesUpdate        => piles.asJson.mapObject(_.add("event", "piles".asJson))
    case playersHands: PlayersHands        => playersHands.asJson.mapObject(_.add("event", "players_hands".asJson))
    case draw: DrawCard            => draw.asJson.mapObject(_.add("event", "draw_card".asJson))
    case play: PlayCard            => play.asJson.mapObject(_.add("event", "play_card".asJson))
    case target: TargetPlayer            => target.asJson.mapObject(_.add("event", "target_player".asJson))
    case bury: BuryCard           => bury.asJson.mapObject(_.add("event", "bury_card".asJson))
    case choose: ChooseCard           => choose.asJson.mapObject(_.add("event", "choose_card".asJson))
    case garbage: GarbageCollect           => garbage.asJson.mapObject(_.add("event", "garbage_collection".asJson))
    case alter: AlterCardOrder           => alter.asJson.mapObject(_.add("event", "alter_the_future".asJson))
    case see: SeeCards           => see.asJson.mapObject(_.add("event", "see_the_future".asJson))
    case nope: ActionNoped           => nope.asJson.mapObject(_.add("event", "noped".asJson))
    case nopes: GetNopes           => nopes.asJson.mapObject(_.add("event", "nope_card".asJson))
    case timer: Timer           => timer.asJson.mapObject(_.add("event", "timer".asJson))
  }
}
