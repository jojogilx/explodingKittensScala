package websockethub

import cats.effect.{IO, Ref}
import cats.effect.std.Queue
import cats.implicits.toTraverseOps
import org.http4s.websocket.WebSocketFrame
import players.Player.PlayerID

trait WebSocketHub {
  def connect(player: PlayerID, queue: Queue[IO, WebSocketFrame.Text]): IO[Unit]

  def sendToPlayer(player: PlayerID, message: Message): IO[Unit]

  def sendToGame(messageFrame: WebSocketFrame): IO[Unit]

  def broadcast(message: String): IO[Unit]

  def disconnectPlayer(playerID: PlayerID): IO[Unit]
}

type Message = String

object WebSocketHub {
  def of: IO[WebSocketHub] = for {
    stateRef <- Ref.of[IO, Map[PlayerID, Queue[IO, WebSocketFrame]]](Map.empty)
    systemQueue <- Queue.unbounded[IO,WebSocketFrame.Text]
  } yield new WebSocketHub {

    override def connect(player: PlayerID, queue: Queue[IO, WebSocketFrame.Text]): IO[Unit] =
      stateRef.update(_ + (player -> queue))

    override def sendToPlayer(playerID: PlayerID, message: Message): IO[Unit] =
      stateRef.get.flatMap { messageMap =>
        messageMap.get(playerID) match {
          case Some(queue) =>
            queue.offer(WebSocketFrame.Text(message)).void
          case None =>
            IO.println(s"Message queue not found for player $playerID")
        }
      }


    override def broadcast(message: Message): IO[Unit] =
      stateRef.get.flatMap(map =>
        map.values.toList.traverse { queue =>
          val a = queue.offer(WebSocketFrame.Text(message))
          a
        }.void
      )

    override def sendToGame(messageFrame: WebSocketFrame): IO[Unit] = {
      case WebSocketFrame.Text(message, _) => systemQueue.offer(WebSocketFrame.Text(message))
    }

    override def disconnectPlayer(playerID: PlayerID): IO[Unit] = {
      stateRef.update(_ - playerID)

    }

  }
}

