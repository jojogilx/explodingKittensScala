package websockethub

import cats.effect.{IO, Ref}
import cats.effect.std.Queue
import cats.implicits.toTraverseOps
import org.http4s.websocket.WebSocketFrame
import players.Player.PlayerID

trait WebSocketHub {
  type Message = String
  def connect(player: PlayerID, queue: Queue[IO, WebSocketFrame.Text], onDisconnected: IO[Unit]): IO[Unit]

  def sendToPlayer(player: PlayerID, message: Message): IO[Unit]

  def sendToGame(message: Message): IO[Unit]
  def getGameInput: IO[String]

  def broadcast(message: String): IO[Unit]

  def disconnectPlayer(playerID: PlayerID): IO[Unit]
}

object WebSocketHub {
  def of: IO[WebSocketHub] = for {
    stateRef <- Ref.of[IO, Map[PlayerID, (Queue[IO, WebSocketFrame.Text], IO[Unit])]](Map.empty)
    systemQueue <- Queue.unbounded[IO, String]
  } yield new WebSocketHub {


    override def connect(player: PlayerID, queue: Queue[IO, WebSocketFrame.Text], onDisconnected: IO[Unit]): IO[Unit] = {
      println(s"$player connected")
      stateRef.update(_ + (player -> (queue, onDisconnected)))
    }

    override def sendToPlayer(playerID: PlayerID, message: Message): IO[Unit] = {
      println(message)


      stateRef.get.flatMap { messageMap =>
        messageMap.get(playerID) match {
          case Some((queue, _)) =>
            queue.offer(WebSocketFrame.Text(message)) *>
              {if(message.contains("\n")) queue.offer(WebSocketFrame.Text(" ")).void else IO.unit}
          case None =>
            IO.println(s"Message queue not found for player $playerID")
        }
      }
    }

    override def broadcast(message: Message): IO[Unit] = {
      println(message)
      stateRef.get.flatMap(map =>
        map.values.toList.traverse { case (queue,_) =>
          queue.offer(WebSocketFrame.Text(message)) *>
            {if(message.contains("\n")) queue.offer(WebSocketFrame.Text(" ")).void else IO.unit}
        }.void
      )
    }

    override def sendToGame(message: String): IO[Unit] = {
      println(message)
      systemQueue.offer(message)
    }

    override def disconnectPlayer(playerID: PlayerID): IO[Unit] = {
      IO.println(s"$playerID disconnected from socket") *> stateRef.get.flatMap(_.get(playerID).fold(IO.unit){ case (_,onDisc) => onDisc}
      ) *> stateRef.update(_ - playerID)

    }

    override def getGameInput: IO[String] =
      systemQueue.take.map(_.replaceAll("\n","").trim)
  }
}

