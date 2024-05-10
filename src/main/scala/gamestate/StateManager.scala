package gamestate

import card._
import cats.effect.IO
import cats.effect.std.Queue
import cats.implicits._
import gamestate.Command._
import players.Player
import players.Player.PlayerID
import websockethub.Event._
import websockethub.WebSocketHub

import scala.util.Random

trait StateManager {
  def tell(command: Command): IO[Unit]

  def ask(command: Command): IO[State]

}

object StateManager {
  def of(initialState: State, webSocketHub: WebSocketHub): IO[StateManager] =
    for {
      commandQueue <- Queue.unbounded[IO, Option[Envelope]]
      _ <- fs2.Stream
        .fromQueueNoneTerminated[IO, Envelope](commandQueue)
        .evalScan(initialState) { (state, envelope) =>
          {
            def updateDiscardDeck(function: Deck => Deck): State =
              state.copy(discardDeck = function(state.discardDeck))

            def handleCardDrawn(card: Card, playerID: PlayerID): IO[Unit] = card match {
              case ExplodingKitten =>
                webSocketHub.broadcast(DrawCardEvent(card, playerID.some)) *> commandQueue.offer(
                  Envelope(TryDefuse(playerID), none).some
                )
              case _ => webSocketHub.sendToPlayer2(playerID)(DrawCardEvent(card, none))
            }

            val (newState, effect) = envelope.command match {
              case SetRandomPlayerTurn() =>
                val index  = Random.nextInt(state.players.length)
                val player = state.players(index).playerID
                (
                  state.copy(currentPlayerIndex = index),
                  webSocketHub.broadcast(Information(s"$player's starting")) *> webSocketHub.broadcast(NewTurn(player))
                )

              case AddPlayer(player) =>
                val newPlayer      = Player(player)
                val updatedPlayers = newPlayer :: state.players

                (state.copy(players = updatedPlayers), IO.unit)

              case RemovePlayer(player) =>
                val updatedPlayers = state.players.filterNot(_.playerID == player)
                (state.copy(players = updatedPlayers), IO.unit)

              case KillCurrentPlayer() =>
                val currentIndex  = state.currentPlayerIndex
                val (left, right) = state.players.splitAt(currentIndex)
                val newPlayers    = left ::: right.drop(1)

                (
                  state.copy(players = newPlayers),
                  webSocketHub.broadcast(Information(s"${right.head} died")) *> commandQueue.offer(
                    Envelope(NextPlayerTurn(), none).some
                  )
                ) // > avoid head

              case PlayCard(playerID, index) =>
                val card          = state.playersHands(playerID)(index)
                val (left, right) = state.playersHands(playerID).splitAt(index)
                val newCards      = left ::: right.drop(1)

                (
                  state.copy(
                    discardDeck = state.discardDeck.prepend(card),
                    playersHands = state.playersHands + (playerID -> newCards)
                  ),
                  webSocketHub.broadcast(PlayCardEvent(card))
                )

              case DrawCard(playerID) =>
                val (deck, card) = state.drawDeck.draw
                val cards        = state.playersHands(playerID)

                (
                  state.copy(
                    drawDeck = deck,
                    playersHands = state.playersHands + (playerID -> (card :: cards))
                  ),
                  handleCardDrawn(card, playerID)
                )

              case TryDefuse(playerID) =>
                val hand = state.playersHands(playerID)

                val defuseOpt = hand.zipWithIndex.find { case (c, _) => c == Defuse }

                (
                  state,
                  defuseOpt.fold(
                    webSocketHub.broadcast(Information("No defuse found")) *> commandQueue.offer(
                      Envelope(KillCurrentPlayer(), none).some
                    )
                  )(card =>
                    webSocketHub.broadcast(Information("Card defused")) *> commandQueue.offer(
                      Envelope(PlayCard(playerID, card._2), none).some
                    )
                  )
                )

            }

            for {
              _ <- effect
              _ <- envelope.reply.traverse_ { deferred =>
                deferred.complete(newState)
              }
            } yield newState
          }
        }
        .compile
        .drain
        .start
    } yield new StateManager {
      def tell(command: Command): IO[Unit] = {
        val envelope = Envelope(command, None)
        commandQueue.offer(Some(envelope))
      }

      def ask(command: Command): IO[State] = {
        for {
          deferred <- IO.deferred[State]
          envelope = Envelope(command, Some(deferred))
          _     <- commandQueue.offer(Some(envelope))
          event <- deferred.get
        } yield event
      }

    }
}
