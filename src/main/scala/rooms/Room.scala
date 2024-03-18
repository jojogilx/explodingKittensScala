package rooms

import cats.effect.std.Queue
import cats.effect.{IO, Ref}
import cats.implicits.catsSyntaxApplicativeId
import game.Game
import org.http4s.websocket.WebSocketFrame
import players.Player._
import websockethub.WebSocketHub

case class Room(webSocketHub: WebSocketHub, game: Game, name: String, nPlayers: Int, stateRef: Ref[IO, RoomState]) {
  def join(playerID: PlayerID, queue: Queue[IO,WebSocketFrame.Text]): IO[Either[String, String]] =
    for {
      currentPlayers <- stateRef.get.map(_.players)
      started <- stateRef.get.map(_.started)
      _ <- webSocketHub.connect(playerID, queue, game.playerDisconnected(playerID))
      res <-
        if (currentPlayers.length == nPlayers) Left("This room is full").pure[IO]
        else if (started) Left("The game already started").pure[IO]
        else if (currentPlayers.contains(playerID)) Left(s"ID $playerID already exists in this room").pure[IO]
        else
          game.joinGame(playerID) *> stateRef.update(roomState => roomState.copy(players = playerID :: roomState.players)) *> Right("Room created")
            .pure[IO]
    } yield res

  def leave(playerID: PlayerID): IO[Unit] =
    game.playerDisconnected(playerID) *> stateRef.update(roomState => roomState.copy(players = roomState.players.filterNot(_ == playerID))) *> webSocketHub.disconnectPlayer(playerID)

  def startGame: IO[Either[String,IO[Unit]]] =
    for {
      nCurrentPlayers <- stateRef.get.map(_.players.length)
      res <-
        if (nPlayers != nCurrentPlayers) Left(s"Not enough players ($nCurrentPlayers/$nPlayers)").pure[IO]
        else stateRef.update(roomState => roomState.copy(started = true)) *> Right(game.initialize()).pure[IO]
    } yield res

  def sendToGame(message: String): IO[Unit] = {
    webSocketHub.sendToGame(message)
  }

}

object Room {
  def create(nPlayers: Int, name: String): IO[Room] = {
    for {
      webSocketHub <- WebSocketHub.of
      game         <- Game.create(nPlayers, webSocketHub)
      state        <- Ref.of[IO, RoomState](RoomState(List.empty, started = false))
    } yield new Room(webSocketHub, game, name, nPlayers, state)
  }
}

case class RoomState(players: List[PlayerID], started: Boolean)
