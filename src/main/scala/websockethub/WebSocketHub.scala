package websockethub

import cats.effect.{Deferred, IO, Ref}
import cats.effect.std.Queue
import cats.implicits.{catsSyntaxOptionId, toFoldableOps, toTraverseOps}
import org.http4s.websocket.WebSocketFrame
import players.Player.PlayerID
import fs2._

import scala.concurrent.duration.DurationInt

trait WebSocketHub {
  type Message = String
  def connect(player: PlayerID, queue: Queue[IO, WebSocketFrame], onDisconnected: IO[Unit]): IO[Unit]

  def sendToPlayer(player: PlayerID)(message: Message): IO[Unit]
  def broadcastExcept(playerID: PlayerID, message: Message): IO[Unit]
  def sendToGame(playerID: PlayerID, message: Message): IO[Unit]
  def getGameInput(playerID: PlayerID): IO[String]

  def broadcast(message: String): IO[Unit]
  def sendToHost(message: String): IO[Unit]
  def receiveFromHost(): IO[String]
  def endGame(): IO[Unit]

  def getPendingInputs(players: List[PlayerID], message: Message): IO[Option[PlayerID]]

  def disconnectPlayer(playerID: PlayerID): IO[Unit]
}

object WebSocketHub {
  def of: IO[WebSocketHub] = for {
    hostRef          <- Ref.of[IO, String]("")
    stateRef         <- Ref.of[IO, Map[PlayerID, (Queue[IO, WebSocketFrame], IO[Unit])]](Map.empty)
    systemQueue      <- Queue.unbounded[IO, (PlayerID, String)]
    pendingInputsRef <- Ref.of[IO, Map[PlayerID, Deferred[IO, String]]](Map.empty)

  } yield new WebSocketHub {

    override def connect(
        player: PlayerID,
        queue: Queue[IO, WebSocketFrame],
        onDisconnected: IO[Unit]
    ): IO[Unit] = {
      println(s"$player connected")

      hostRef.get
        .map(_.isBlank)
        .flatMap(b => if (b) hostRef.update(_ => player) *> IO.println(s"host is $player") else IO.unit) *> stateRef
        .update(_ + (player -> (queue, onDisconnected)))
    }

    override def sendToPlayer(playerID: PlayerID)(message: Message): IO[Unit] = {

      stateRef.get.flatMap { messageMap =>
        messageMap.get(playerID) match {
          case Some((queue, _)) =>
            message.split("\n").map(msg => queue.offer(WebSocketFrame.Text(msg)) *> queue.offer(WebSocketFrame.Text(" "))).toList.traverse_(identity)
          case None =>
            IO.println(s"Message queue not found for player $playerID")
        }
      }
    }



    override def broadcast(message: Message): IO[Unit] = {


      stateRef.get.flatMap(map =>
        map.values.toList.traverse { case (queue, _) =>
          message.split("\n").map(msg => queue.offer(WebSocketFrame.Text(msg))*> queue.offer(WebSocketFrame.Text(" "))).toList.traverse_(identity)
        }.void
      )
    }

    override def broadcastExcept(playerID: PlayerID, message: Message): IO[Unit] = {
      stateRef.get.flatMap(map =>
        map
          .filterNot(_._1 == playerID)
          .values
          .toList
          .traverse { case (queue, _) =>
            message.split("\n").map(msg => queue.offer(WebSocketFrame.Text(msg))*> queue.offer(WebSocketFrame.Text(" "))).toList.traverse_(identity)
          }
          .void
      )
    }

    override def sendToGame(playerID: PlayerID, message: String): IO[Unit] = {
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

    override def getPendingInputs(players: List[PlayerID], message: Message): IO[Option[PlayerID]] =
      for {
        deferred  <- Deferred[IO, Option[PlayerID]]
        deferreds <- players.traverse(_ => Deferred[IO, String])
        map = players.zip(deferreds).toMap
        _ <- pendingInputsRef.update(_ => map)
        _ <- IO.race(
          Stream
            .repeatEval(systemQueue.take)
            .evalMap({
              case (id, str) if str.trim.toLowerCase.replaceAll("\n", "") == message =>
                deferred.complete(id.some) *> IO.pure(true)
              case (id, str) =>
                pendingInputsRef.get.map(map => map(id)).flatMap(deferred => deferred.complete(id)) *> IO.pure(false)
            })
            .takeWhile(!_)
            .compile
            .drain,
          pendingInputsRef.get.flatMap(map => map.values.toList.traverse(_.get) *> deferred.complete(None))
        )
        res <- deferred.get
      } yield res

    override def endGame(): IO[Unit] =
      stateRef.get.flatMap(map =>
        map.values.toList.traverse { case (queue, _) =>
          queue.offer(WebSocketFrame.Close())
        }.void
      )

    override def sendToHost(message: Message): IO[Unit] =
      hostRef.get.flatMap(playerID => sendToPlayer(playerID)(message)
      )

    override def receiveFromHost(): IO[String] =
      hostRef.get.flatMap(playerID => getGameInput(playerID))
  }

}
