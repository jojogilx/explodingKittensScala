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
  def sendToPlayer(player: PlayerID)(event: Event): IO[Unit]
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
    systemQueuesMap <- Ref.of[IO, Map[PlayerID, Queue[IO, String]]](Map.empty)
  } yield new WebSocketHub {

    override def connect(
        player: PlayerID,
        queue: Queue[IO, WebSocketFrame],
        onDisconnected: IO[Unit]
    ): IO[Unit] =
        for {
          _ <-   stateRef.update(map => if(!map.contains(player)) map + (player -> (queue, onDisconnected)) else map)
          q <- Queue.unbounded[IO, String]
          _ <- systemQueuesMap.update(map => map + (player -> q))
          _ <- IO.println(s"$player connected")
        } yield ()


    override def sendToPlayer(playerID: PlayerID)(event: Event): IO[Unit] = {

      stateRef.get.flatMap { messageMap =>
        messageMap.get(playerID) match {
          case Some((queue, _)) =>
            queue.offer(WebSocketFrame.Text(event.asJson.noSpaces))
          case None =>
            IO.println(s"Message queue not found for player $playerID")
        }
      }
    }




    override def broadcast(event: Event): IO[Unit] = {
      stateRef.get.flatMap(map =>
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

    override def sendToGame(playerID: PlayerID)(message: String): IO[Unit] =
      for {
        queue <- systemQueuesMap.get.map(_.get(playerID))
        _ <- queue.fold(IO.unit)(q => q.offer(message))
      } yield ()

    override def disconnectPlayer(playerID: PlayerID): IO[Unit] = {
      IO.println(s"$playerID disconnected from socket") *> stateRef.get.flatMap(_.get(playerID).fold(IO.unit) {
        case (_, onDisc) => onDisc
      }) *> stateRef.update(_ - playerID)

    }

    override def getGameInput(playerID: PlayerID): IO[String] =
        for {
          queue <- systemQueuesMap.get.map(_(playerID))
          msg <- Stream
                .repeatEval(queue.take).map(_.replaceAll("\n", "").trim.toLowerCase).collectFirst({ case message if message.nonEmpty => message }).compile.lastOrError

        } yield msg



    override def endGame(): IO[Unit] =
      stateRef.get.flatMap(map =>
        map.values.toList.traverse { case (queue, _) =>
          queue.offer(WebSocketFrame.Close())
        }.void
      )


  }

}
