package gamestate

import card.{Card, Deck}
import cats.effect.IO
import cats.effect.std.Queue
import cats.implicits.{toFoldableOps, toFunctorOps}
import gamestate.Command._
import players.Player
import utils.TerminalUtils.{PlayerColors, colorPlayerMessage, colorSystemMessage, diedMessage}
import websockethub.WebSocketHub

trait StateManager {
  def tell(envelope: Envelope): IO[Unit]

  def ask(envelope: Envelope): IO[State]

}

object StateManager {
  def of(initialState: State, webSocketHub: WebSocketHub): IO[StateManager] =
    for {
      commandQueue <- Queue.unbounded[IO, Option[Envelope]]
      _ <- fs2.Stream
        .fromQueueNoneTerminated[IO, Envelope](commandQueue)
        .evalScan(initialState) { (state, envelope) =>
          {

            def switchPiles(): State = {
              val draw = Deck.initShuffledFromDiscardPile2(state.drawDeck, state.discardDeck)

              webSocketHub.broadcast(colorSystemMessage(s"switching piles"))
              state.copy(drawDeck = draw, discardDeck = Deck(List.empty))
            }

            // validate
            // updateState
            // effect

            val newState = envelope.command match {
              case AddPlayer(player) =>
                val newPlayer      = Player(player, PlayerColors(state.players.length))
                val updatedPlayers = newPlayer :: state.players
                webSocketHub.broadcast(colorSystemMessage(s"$player joined the game"))
                state.copy(players = updatedPlayers)

              case RemovePlayer(player) =>
                val updatedPlayers = state.players.filterNot(_.playerID == player)
                webSocketHub.broadcast(colorSystemMessage(s"$player left the game"))
                state.copy(players = updatedPlayers)

              case KillCurrentPlayer() =>
                val currentIndex  = state.currentPlayerIndex
                val (left, right) = state.players.splitAt(currentIndex)
                val newPlayers    = left ::: right.drop(1)

                webSocketHub.broadcast(diedMessage(right.head)) // > avoid head
                state.copy(players = newPlayers)

              case PlayCard(playerID, index) =>
                val card          = state.playersHands(playerID)(index)
                val (left, right) = state.playersHands(playerID).splitAt(index)
                val newCards      = left ::: right.drop(1)

                state.copy(
                  discardDeck = state.discardDeck.prepend(card),
                  playersHands = state.playersHands + (playerID -> newCards)
                )

              case DrawCard(playerID) =>
                val newState =
                  if (state.drawDeck.length == 0)
                    switchPiles()
                  else
                    state

                val (deck, card) = newState.drawDeck.draw
                val cards          = state.playersHands(playerID)

                newState.copy(
                  drawDeck = deck,
                  playersHands = newState.playersHands + (playerID -> card::cards)
                )

            }

            for {
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
