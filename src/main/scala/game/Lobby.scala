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
import scala.concurrent.duration.DurationInt

object Lobby extends IOApp {
  // (max rooms)
  // create room
  // join room
  private def httpApp: IO[WebSocketBuilder2[IO] => HttpApp[IO]] = {
    for {
      playersRef  <- Ref.of[IO, Map[PlayerID, UUID]](Map.empty)
      messagesRef <- Ref.of[IO, Map[String, Queue[IO, WebSocketFrame]]](Map.empty)
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
                messagesRef.get.flatMap { messageMap =>
                  messageMap.get(playerID) match {
                    case Some(queue) =>
                      queue.offer(WebSocketFrame.Text(message)).void
                    case None =>
                      IO.println(s"Message queue not found for player $playerID")
                  }
                }
        }

        val game = Game(5, broadcastMessage, sendMessage)

        HttpRoutes
          .of[IO] {
            // print current room and list of players
            // command createRoom name > creates room
            // command join roomName > sends get to /room if there is one


            case GET -> Root / "join" / name =>

          /*    def getNumberOfPlayers(send: Queue[IO, WebSocketFrame], receive: Queue[IO, String]): IO[Int] =
                for {
                  _ <- send.offer(WebSocketFrame.Text(s"Please insert number of players (2-5) >> "))
                  num <- receive.take.flatMap(_.toIntOption match {
                    case Some(x) =>
                      x match {
                        case i if (2 to 5) contains i => i.pure[IO]

                        case _ => send.offer(WebSocketFrame.Text(s"${RedText}Invalid number of players$ResetText\n")) *> getNumberOfPlayers(send, receive)
                      }
                    case None => send.offer(WebSocketFrame.Text(s"${RedText}Invalid input$ResetText\n")) *> getNumberOfPlayers(send, receive)
                  })
                } yield num
*/


              for {
                uid <- IO.randomUUID
                q   <- Queue.unbounded[IO, WebSocketFrame]
                _   <- messagesRef.update(map => map + (name -> q))
                _   <- playersRef.update(map => map + (name -> uid))
                recQ <- Queue.unbounded[IO, String]
                _ <- game.joinGame(name, recQ)
                w <- wsb.build(
                  receive = _.evalMap({
                    case WebSocketFrame.Text(string, _) => recQ.offer(string)
                  }),
                  send = Stream.repeatEval(q.take)
                    .merge(Stream.awakeEvery[IO](5.seconds)
                      .map(_ => WebSocketFrame.Text(colorSystemMessage(s"Waiting for players...")))
                      .interruptWhen(Stream.repeatEval(playersRef.get.map(_.size >= 5)))
                    )
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
        .withPort(port"8080")
        .withHttpWebSocketApp(_)
        .build
        .useForever
    }
}
