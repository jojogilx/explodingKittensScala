package game

import cats.effect._
import cats.effect.std.Queue
import cats.implicits._
import com.comcast.ip4s.IpLiteralSyntax
import fs2._
import org.http4s._
import org.http4s.dsl.io._
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.middleware.ErrorHandling
import org.http4s.server.websocket.WebSocketBuilder2
import org.http4s.websocket.WebSocketFrame
import players.Player
import players.Player.PlayerID
import rooms.Room
import rooms.Room._
import utils.TerminalUtils._

import java.util.UUID

object Lobby extends IOApp {
  // (max rooms)
  // create room
  // join room
  private def httpApp: IO[WebSocketBuilder2[IO] => HttpApp[IO]] = {
    for {
      playersRef  <- Ref.of[IO, Map[PlayerID, UUID]](Map.empty)
      messagesRef <- Ref.of[IO, Map[UUID, Queue[IO, WebSocketFrame]]](Map.empty)
    } yield { wsb: WebSocketBuilder2[IO] =>
      {

        def broadcastMessage(message: String): IO[Unit] = { // inject this function into game
          messagesRef.get.flatMap(map =>
            map.values.toList.traverse { queue =>
              val a = queue.offer(WebSocketFrame.Text(message))
              a
            }.void
          )
        }

        def sendMessage(playerID: PlayerID, message: String): IO[Unit] = {
          playersRef.get.flatMap { playerMap =>
            playerMap.get(playerID) match {
              case Some(uuid) =>
                messagesRef.get.flatMap { messageMap =>
                  messageMap.get(uuid) match {
                    case Some(queue) =>
                      queue.offer(WebSocketFrame.Text(message)).void
                    case None =>
                      IO.println(s"Message queue not found for player $playerID")
                  }
                }
              case None =>
                IO.println(s"Player not found with ID $playerID")
            }
          }
        }

        val game = Game(5, broadcastMessage, sendMessage)

        HttpRoutes
          .of[IO] {
            // print current room and list of players
            // command createRoom name > creates room
            // command join roomName > sends get to /room if there is one

            // curl -XPOST "localhost:8080/json" -d 'playerName' -H "Content-Type: application/json"

            case GET -> Root / "join" / name =>
              for {
                uid <- IO.randomUUID
                q   <- Queue.unbounded[IO, WebSocketFrame]
                _   <- messagesRef.update(map => map + (uid -> q))
                _   <- playersRef.update(map => map + (name -> uid))
                _ <- game.joinGame(name)
                w <- wsb.build(
                  receive = _.void,
                  send = /*Stream.emit("What's your name?").map(WebSocketFrame.Text(_)) ++*/
                    Stream.repeatEval(q.take)
                )

              } yield w

            case GET -> Root / "start" =>
              for {
                w        <- game.initialize()
                response <- Accepted()
              } yield response

          }
          .orNotFound

      }
    }
  }

  // Run the lobby server
  override def run(args: List[String]): IO[ExitCode] =
    httpApp.flatMap {
      EmberServerBuilder
        .default[IO]
        .withHost(ipv4"127.0.0.1")
        .withPort(port"8084")
        .withHttpWebSocketApp(_)
        .build
        .useForever
    }
}
