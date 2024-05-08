package rooms

import card.Recipe
import cats.effect._
import cats.effect.std.Queue
import cats.implicits.catsSyntaxApplicativeId
import game.Game
import org.http4s.websocket.WebSocketFrame
import players.Player._
import websockethub.Event._
import websockethub.WebSocketHub




case class Room(webSocketHub: WebSocketHub, game: Game, name: String, stateRef: Ref[IO, RoomState]) {

  /**
   * Joins a player to this room
   * @param playerID player's id
   * @param queue queue to connect to the websockethub that will serve to communicate game<->player
   * @return either a string with the failure or a string with room created
   */
  def join(playerID: PlayerID, queue: Queue[IO,WebSocketFrame]): IO[Either[String, Unit]] =
    for {
      currentPlayers <- stateRef.get.map(_.players)
      started <- stateRef.get.map(_.started)
      res <- if(playerID.isEmpty) Left("no player Id").pure[IO]
        else if (currentPlayers.length == 5) Left("This room is full").pure[IO]
        else if (started) Left("The game already started").pure[IO]
        else {
          for {
            _ <- if(currentPlayers.contains(playerID)) {
              for {
                _<-  webSocketHub.connect(playerID, queue, game.playerDisconnected(playerID))
                _ <- game.reconnect(playerID)
                state <- stateRef.get
                _ <- webSocketHub.sendToPlayer2(playerID)(RoomStateEvent(state.seatings.toList, state.recipe))

              } yield ()
            }
            else {
              for {
                _ <- webSocketHub.connect(playerID, queue, game.playerDisconnected(playerID))
                state <- stateRef.updateAndGet(roomState => {
                  val newPlayers =  roomState.players :+ playerID
                  val newSeatings = roomState.seatings + (playerID -> roomState.seatings.size)
                  roomState.copy(players = newPlayers, seatings = newSeatings)
                })
                _ <- game.joinGame(playerID)
                _ <- webSocketHub.broadcast(Joined(playerID, state.seatings.toList))
                _ <- webSocketHub.sendToPlayer2(playerID)(RoomStateEvent(state.seatings.toList, state.recipe))
              } yield ()
            }
          } yield Right()
        }
    } yield res

  /**
   * Called when a player leaves the room
   * @param playerID id of the player that is leaving
   */
  def leave(playerID: PlayerID): IO[Unit] =
    for {
      _ <- game.playerDisconnected(playerID)
      state <- stateRef.updateAndGet(roomState => roomState.copy(players = roomState.players.filterNot(_ == playerID)))
      _ <- webSocketHub.disconnectPlayer(playerID)
      _ <- webSocketHub.broadcast(LeftGame(playerID, state.seatings.toList))
    } yield ()


  /**
   * If the room is full, starts the game
   * @return either a string representing a failure or initializes game
   */
  def startGame: IO[Either[String,IO[Unit]]] =
    for {
      nCurrentPlayers <- stateRef.get.map(_.players.length)
      res <-
        if (0 == nCurrentPlayers) Left(s"Not enough players ($nCurrentPlayers/0)").pure[IO]
        else /*stateRef.update(roomState => roomState.copy(started = true)) *> */Right(game.initialize()).pure[IO]
    } yield res

  def startGame2: IO[Unit] =
    for {
      state <- stateRef.get
      res <-
        if (0 == state.players.length) webSocketHub.broadcast(Information(s"Not enough players (${state.players.length}/0)"))
        else if(state.started) IO.unit
        else stateRef.update(roomState => roomState.copy(started = true)) *> game.initialize()
    } yield res

  /**
   * Sends a message to the game
   * @param playerID id of the player that sent the message
   * @param message message to send
   */
  def sendToGame(playerID: PlayerID)(message: String): IO[Unit] = {
      message match {
        case "started" => startGame2.flatMap(_ => IO.unit)
        case "left" => IO.println("player left")
        case _ => IO.println(message) *> webSocketHub.sendToGame(playerID, message)
      }
  }


}

object Room {

  /**
   * Creates a new room with chosen number of players and name
   * @param nPlayers number of players
   * @param name name of the room
   * @return room created
   */
  def create(name: String, recipe: Recipe): IO[Room] = {
    for {
      webSocketHub <- WebSocketHub.of
      game         <- Game.create(5, webSocketHub, recipe)
      state        <- Ref.of[IO, RoomState](RoomState(List.empty, Map.empty, started = false, recipe))
    } yield new Room(webSocketHub, game, name, state)
  }
}

case class RoomState(players: List[PlayerID], seatings: Map[PlayerID, Int], started: Boolean, recipe: Recipe)
