package gamestate

import card.{Card, Deck}
import cats.effect.{Deferred, IO}
import players.Player._



sealed trait Command

object Command {
  final case class AddPlayer(playerID: PlayerID) extends Command
  final case class RemovePlayer(playerID: PlayerID)  extends Command

  final case class NextPlayerTurn()  extends Command
  final case class PreviousPlayerTurn()  extends Command
  final case class SetPlayerTurn(playerID: PlayerID)  extends Command
  final case class SetRandomPlayerTurn()  extends Command

  final case class DrawCard(card: Card) extends Command
  final case class DiscardCard(card: Card) extends Command

}


final case class Envelope(command: Command, effect: IO[Unit], reply: Option[Deferred[IO, State]])