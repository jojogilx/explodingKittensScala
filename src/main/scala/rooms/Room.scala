package rooms

import cats.effect.{IO, Ref}
import game.Game
import players.Player._
import websockethub.WebSocketHub

case class Room(game: Game, name: String, nPlayers: Int, stateRef: Ref[IO, RoomState]) {
  def join(playerID: PlayerID): IO[Unit] =
    for {
      nCurrentPlayers <- stateRef.get.map(_.players.length)
      _ <- if (nCurrentPlayers == nPlayers) IO.println("This room is full") else
        stateRef.update(roomState => roomState.copy(players = playerID :: roomState.players))
    } yield ()

  def leave(playerID: PlayerID): IO[Unit] =
    stateRef.update(roomState => roomState.copy(players = roomState.players.filterNot(_ == playerID)))

  def startGame: IO[Unit] =
    for {
      nCurrentPlayers <- stateRef.get.map(_.players.length)
      _ <- if(nPlayers != nCurrentPlayers) IO.println(s"Not enough players ($nCurrentPlayers/$nPlayers)")
      else stateRef.update(roomState => roomState.copy(started = true)) *> game.initialize()
    } yield ()

}

object Room {
  def create(nPlayers: Int, name: String): IO[Room] = {
    for {
      webSocketHub <- WebSocketHub.of
      game         <- Game.create(nPlayers, webSocketHub)
      state <- Ref.of[IO, RoomState](RoomState(List.empty,started = false))
    } yield new Room(game, name,nPlayers, state)
  }
}


case class RoomState(players: List[PlayerID], started: Boolean)