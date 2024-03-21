package rooms

import cats.effect.std.Queue
import cats.effect.{Deferred, IO, Ref}
import cats.implicits.catsSyntaxApplicativeId
import game.Game
import org.http4s.websocket.WebSocketFrame
import players.Player._
import utils.TerminalUtils.{GreenText, RedText, ResetText}
import websockethub.WebSocketHub

case class Room(webSocketHub: WebSocketHub, game: Game, name: String, nPlayers: Int, stateRef: Ref[IO, RoomState], deferred: Deferred[IO,Boolean]) {

  /**
   * Joins a player to this room
   * @param playerID player's id
   * @param queue queue to connect to the websockethub that will serve to communicate game<->player
   * @return either a string with the failure or a string with room created
   */
  def join(playerID: PlayerID, queue: Queue[IO,WebSocketFrame.Text]): IO[Either[String, Deferred[IO,Boolean]]] =
    for {
      currentPlayers <- stateRef.get.map(_.players)
      started <- stateRef.get.map(_.started)
      res <-
        if (currentPlayers.length == nPlayers) Left("This room is full").pure[IO]
        else if (started) Left("The game already started").pure[IO]
        else if (currentPlayers.contains(playerID)) Left(s"ID $playerID already exists in this room").pure[IO]
        else {
          for {
            _ <- webSocketHub.connect(playerID, queue, game.playerDisconnected(playerID))
            _ <- stateRef.update(roomState => roomState.copy(players = playerID :: roomState.players))
            _ <- game.joinGame(playerID)
          } yield Right(deferred)
        }
    } yield res

  /**
   * Called when a player leaves the room
   * @param playerID id of the player that is leaving
   */
  def leave(playerID: PlayerID): IO[Unit] =
    game.playerDisconnected(playerID) *> stateRef.update(roomState => roomState.copy(players = roomState.players.filterNot(_ == playerID))) *> webSocketHub.disconnectPlayer(playerID)

  /**
   * If the room is full, starts the game
   * @return either a string representing a failure or initializes game
   */
  def startGame: IO[Either[String,IO[Unit]]] =
    for {
      nCurrentPlayers <- stateRef.get.map(_.players.length)
      res <-
        if (nPlayers != nCurrentPlayers) Left(s"Not enough players ($nCurrentPlayers/$nPlayers)").pure[IO]
        else stateRef.update(roomState => roomState.copy(started = true)) *> Right(game.initialize()).pure[IO]
    } yield res

  /**
   * Sends a message to the game
   * @param playerID id of the player that sent the message
   * @param message message to send
   */
  def sendToGame(playerID: PlayerID)(message: String): IO[Unit] = {
    webSocketHub.sendToGame(playerID, message)
  }

  /**
   * Returns a string representation of this room, with name and players. If joinable text is green else red
   * @return string representation
   */
  def getString: IO[String] = for {
    room <- stateRef.get
    color = if (room.started || room.players.length == nPlayers) RedText else GreenText

  } yield s"$color $name,  Players(${room.players.length}/$nPlayers): ${room.players}$ResetText\n"


}

object Room {

  /**
   * Creates a new room with chosen number of players and name
   * @param nPlayers number of players
   * @param name name of the room
   * @return room created
   */
  def create(nPlayers: Int, name: String): IO[Room] = {
    for {
      webSocketHub <- WebSocketHub.of
      deferred <- Deferred[IO, Boolean]
      game         <- Game.create(nPlayers, webSocketHub, deferred)
      state        <- Ref.of[IO, RoomState](RoomState(List.empty, started = false))
    } yield new Room(webSocketHub, game, name, nPlayers, state, deferred)
  }
}

case class RoomState(players: List[PlayerID], started: Boolean)
