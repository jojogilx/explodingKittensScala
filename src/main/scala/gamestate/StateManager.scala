package gamestate

import cats.effect.IO
import cats.effect.std.Queue
import cats.implicits.{toFoldableOps, toFunctorOps}
import players.Player
import utils.TerminalUtils.PlayerColors

trait StateManager {
  def tell(envelope: Envelope): IO[Unit]

  def ask(envelope: Envelope): IO[State]

}

object StateManager {
  def of(initialState: State): IO[StateManager] =
    for {
      commandQueue <- Queue.unbounded[IO, Option[Envelope]]
      _ <- fs2.Stream
        .fromQueueNoneTerminated[IO, Envelope](commandQueue)
        .evalScan(initialState) { (state, envelope) =>
          // validate
          // updateState
          // effect

          val newState = envelope.command match {
            case Command.AddPlayer(player) =>
              val newPlayer = Player(player, PlayerColors(state.players.length))
              val updatedPlayers = newPlayer :: state.players
              state.copy(players = updatedPlayers)

            case Command.RemovePlayer(player) => state.copy()
            case Command.DrawCard(deck) => state.copy()
            case Command.DiscardCard(deck) => state.copy()
          }

          for {
            _ <- envelope.reply.traverse_ { deferred =>
              deferred.complete(newState)
            }
          } yield newState
        }
        .compile
        .drain
        .start
    } yield new StateManager {
      def tell(envelope: Envelope): IO[Unit] = {
        commandQueue.offer(Some(envelope))
      }

      def ask(envelope: Envelope): IO[State] = {
        for {
          deferred <- IO.deferred[State]
          newEnv = envelope.copy(reply = Some(deferred))
          _     <- commandQueue.offer(Some(newEnv))
          event <- deferred.get
        } yield event
      }

    }
}