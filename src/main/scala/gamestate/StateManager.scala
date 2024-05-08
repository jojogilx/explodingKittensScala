package gamestate

import card._
import cats.effect.IO
import cats.effect.std.Queue
import cats.implicits._
import gamestate.Command._
import players.Player
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



            def switchPiles(): State = {
              val draw = Deck.initShuffledFromDiscardPile2(state.drawDeck, state.discardDeck)

//              webSocketHub.broadcast(colorSystemMessage(s"switching piles"))
              state.copy(drawDeck = draw, discardDeck = Deck(List.empty))
            }

            val (newState, effect) = envelope.command match {
              case SetRandomPlayerTurn() =>
                val index = Random.nextInt(state.players.length)
                (state.copy(currentPlayerIndex = index),
                  webSocketHub.broadcast(s"${state.players(index)} 's starting"))


              case AddPlayer(player) =>
                val newPlayer      = Player(player)
                val updatedPlayers = newPlayer :: state.players

                (state.copy(players = updatedPlayers), webSocketHub.broadcast(s"$player joined the game"))

              case RemovePlayer(player) =>
                val updatedPlayers = state.players.filterNot(_.playerID == player)
                (state.copy(players = updatedPlayers),webSocketHub.broadcast(s"$player left the game"))

              case KillCurrentPlayer() =>
                val currentIndex  = state.currentPlayerIndex
                val (left, right) = state.players.splitAt(currentIndex)
                val newPlayers    = left ::: right.drop(1)


                (state.copy(players = newPlayers),webSocketHub.broadcast(s"${right.head} died") )// > avoid head

              case PlayCard(playerID, index) =>
                val card          = state.playersHands(playerID)(index)
                val (left, right) = state.playersHands(playerID).splitAt(index)
                val newCards      = left ::: right.drop(1)

                (state.copy(
                  discardDeck = state.discardDeck.prepend(card),
                  playersHands = state.playersHands + (playerID -> newCards)
                ), IO.unit)

              case DrawCard(playerID) =>
                val newState =
                  if (state.drawDeck.length == 0)
                    switchPiles()
                  else
                    state

                val (deck, card) = newState.drawDeck.draw
                val cards          = state.playersHands(playerID)

                (newState.copy(
                  drawDeck = deck,
                  playersHands = newState.playersHands + (playerID -> (card::cards))
                ), IO.unit)

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
        val envelope = Envelope(command,None)
        commandQueue.offer(Some(envelope))
      }

      def ask(command: Command): IO[State] = {
        for {
          deferred <- IO.deferred[State]
          envelope = Envelope(command,Some(deferred))
          _     <- commandQueue.offer(Some(envelope))
          event <- deferred.get
        } yield event
      }

    }
}
