package websockethub

import cats.effect.{IO, Ref}
import cats.effect.std.Queue
import cats.implicits.toTraverseOps
import org.http4s.websocket.WebSocketFrame
import players.Player.PlayerID

trait WebSocketHub {
  type Message = String
  def connect(player: PlayerID, queue: Queue[IO, WebSocketFrame.Text]): IO[Unit]

  def sendToPlayer(player: PlayerID, message: Message): IO[Unit]

  def sendToGame(message: Message): IO[Unit]
  def getGameInput: IO[String]

  def broadcast(message: String): IO[Unit]

  def disconnectPlayer(playerID: PlayerID): IO[Unit]
}

object WebSocketHub {
  def of: IO[WebSocketHub] = for {
    stateRef <- Ref.of[IO, Map[PlayerID, Queue[IO, WebSocketFrame.Text]]](Map.empty)
    systemQueue <- Queue.unbounded[IO, String]
  } yield new WebSocketHub {

    override def connect(player: PlayerID, queue: Queue[IO, WebSocketFrame.Text]): IO[Unit] = {
      println(s"$player connected")
      stateRef.update(_ + (player -> queue))
    }

    override def sendToPlayer(playerID: PlayerID, message: Message): IO[Unit] = {
      println(message)


      stateRef.get.flatMap { messageMap =>
        messageMap.get(playerID) match {
          case Some(queue) =>
            queue.offer(WebSocketFrame.Text(message)).void
          case None =>
            IO.println(s"Message queue not found for player $playerID")
        }
      }
    }

    override def broadcast(message: Message): IO[Unit] = {
      println(message)
      stateRef.get.flatMap(map =>
        map.values.toList.traverse { queue =>
          val a = queue.offer(WebSocketFrame.Text(message))
          a
        }.void
      )
    }

    override def sendToGame(message: String): IO[Unit] = {
      println(message)
      systemQueue.offer(message)
    }

    override def disconnectPlayer(playerID: PlayerID): IO[Unit] = {
      stateRef.update(_ - playerID)

    }

    override def getGameInput: IO[String] =
      systemQueue.take
  }
}

