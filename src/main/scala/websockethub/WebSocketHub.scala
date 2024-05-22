package websockethub

import cats.effect.std.Queue
import cats.effect.{IO, Ref}
import cats.implicits.toTraverseOps
import io.circe.syntax._
import fs2._
import org.http4s.websocket.WebSocketFrame
import players.Player.PlayerID
import websockethub.Event._



trait WebSocketHub {
  def connect(player: PlayerID, queue: Queue[IO, WebSocketFrame], onDisconnected: IO[Unit]): IO[Unit]
  def sendToPlayer2(player: PlayerID)(event: Event): IO[Unit]
  def broadcastExcept(playerID: PlayerID, message: Event): IO[Unit]
  def sendToGame(playerID: PlayerID)(message: String): IO[Unit]
  def getGameInput(playerID: PlayerID): IO[String]
  def broadcast(event: Event): IO[Unit]

  def endGame(): IO[Unit]

  def disconnectPlayer(playerID: PlayerID): IO[Unit]
}

object WebSocketHub {
  def of: IO[WebSocketHub] = for {
    stateRef         <- Ref.of[IO, Map[PlayerID, (Queue[IO, WebSocketFrame], IO[Unit])]](Map.empty)
    systemQueue      <- Queue.unbounded[IO, (PlayerID, String)]
  } yield new WebSocketHub {

    override def connect(
        player: PlayerID,
        queue: Queue[IO, WebSocketFrame],
        onDisconnected: IO[Unit]
    ): IO[Unit] = {
      stateRef.update(map => if(!map.contains(player)) map + (player -> (queue, onDisconnected)) else map) *> IO.println(s"$player connected")
    }


    override def sendToPlayer2(playerID: PlayerID)(event: Event): IO[Unit] = {

      IO.println(s"PLAYER $playerID -> event: ${event.getClass}: $event") *> stateRef.get.flatMap { messageMap =>
        messageMap.get(playerID) match {
          case Some((queue, _)) =>
            queue.offer(WebSocketFrame.Text(event.asJson.noSpaces))
          case None =>
            IO.println(s"Message queue not found for player $playerID")
        }
      }
    }




    override def broadcast(event: Event): IO[Unit] = {
      IO.println(s"event: ${event.getClass}: $event") *> stateRef.get.flatMap(map =>
        map.values.toList.traverse { case (queue, _) =>
          queue.offer(WebSocketFrame.Text(event.asJson.noSpaces))
        }.void
      )
    }


    override def broadcastExcept(playerID: PlayerID, event: Event): IO[Unit] = {
      stateRef.get.flatMap(map =>
        map
          .filterNot(_._1 == playerID)
          .values
          .toList
          .traverse { case (queue, _) =>
            queue.offer(WebSocketFrame.Text(event.asJson.noSpaces))
          }
          .void
      )
    }

    override def sendToGame(playerID: PlayerID)(message: String): IO[Unit] = {
      systemQueue.offer((playerID, message))
    }

    override def disconnectPlayer(playerID: PlayerID): IO[Unit] = {
      IO.println(s"$playerID disconnected from socket") *> stateRef.get.flatMap(_.get(playerID).fold(IO.unit) {
        case (_, onDisc) => onDisc
      }) *> stateRef.update(_ - playerID)

    }

    override def getGameInput(playerID: PlayerID): IO[String] =
      Stream
        .repeatEval(systemQueue.take)
        .collectFirst({ case (id, message) if id == playerID => message })
        .map(_.replaceAll("\n", "").trim.toLowerCase)
        .compile
        .lastOrError


    override def endGame(): IO[Unit] =
      stateRef.get.flatMap(map =>
        map.values.toList.traverse { case (queue, _) =>
          queue.offer(WebSocketFrame.Close())
        }.void
      )


  }

}
