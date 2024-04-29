package rooms

import cats.effect._
import cats.effect.std.Queue
import cats.implicits.catsSyntaxApplicativeId
import game.Game
import io.circe.syntax.EncoderOps
import org.http4s.websocket.WebSocketFrame
import players.Player._
import utils.TerminalUtils.{GreenText, RedText, ResetText}
import websockethub.WebSocketHub




case class Room(webSocketHub: WebSocketHub, game: Game, name: String, stateRef: Ref[IO, RoomState]) {

  //todo: restructure so it asks game which is the nPlayers max & min


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
        else if (currentPlayers.contains(playerID)) Left(s"ID $playerID already exists in this room").pure[IO]
        else {
          for {
            _ <- webSocketHub.connect(playerID, queue, game.playerDisconnected(playerID))
            state <- stateRef.updateAndGet(roomState => roomState.copy(players = playerID :: roomState.players))
            _ <- game.joinGame(playerID)
            _ <- webSocketHub.broadcast(state.players.asJson.noSpaces)
          } yield Right()
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
        if (0 == nCurrentPlayers) Left(s"Not enough players ($nCurrentPlayers/0)").pure[IO]
        else /*stateRef.update(roomState => roomState.copy(started = true)) *> */Right(game.initialize()).pure[IO]
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
    color = if (room.started || room.players.length == 5) RedText else GreenText

  } yield s"$color $name,  Players(${room.players.length}/5): ${room.players}$ResetText\n"


}

object Room {

  /**
   * Creates a new room with chosen number of players and name
   * @param nPlayers number of players
   * @param name name of the room
   * @return room created
   */
  def create(name: String): IO[Room] = {
    for {
      webSocketHub <- WebSocketHub.of
      game         <- Game.create(5, webSocketHub)
      state        <- Ref.of[IO, RoomState](RoomState(List.empty, started = false))
    } yield new Room(webSocketHub, game, name, state)
  }
}

case class RoomState(players: List[PlayerID], started: Boolean)
