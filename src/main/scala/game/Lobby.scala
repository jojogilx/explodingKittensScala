package game

import cats.effect._
import cats.effect.std.Queue
import com.comcast.ip4s.IpLiteralSyntax
import fs2._
import org.http4s._
import org.http4s.dsl.io._
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.websocket.WebSocketBuilder2
import org.http4s.websocket.WebSocketFrame
import utils.TerminalUtils._
import websockethub.WebSocketHub

import scala.concurrent.duration.DurationInt

object Lobby extends IOApp {
  // (max rooms)
  // create room
  // join room
  private def httpApp: IO[WebSocketBuilder2[IO] => HttpApp[IO]] = {
    for {
      webSocketHub <- WebSocketHub.of
      game         <- Game.create(2, webSocketHub)
    } yield { wsb: WebSocketBuilder2[IO] =>
      {

        HttpRoutes
          .of[IO] {
            // print current room and list of players
            // command createRoom name > creates room
            // command join roomName > sends get to /room if there is on

            case GET -> Root / "join" / playerID =>
              for {
                q <- Queue.unbounded[IO, WebSocketFrame.Text]
                _ <- webSocketHub.connect(playerID, q)
                _ <- game.joinGame(playerID)
                w <- wsb
                  .build(
                    receive = _.filter({
                      case WebSocketFrame.Text(_) => true
                      case _                      => false
                    }).map({ case WebSocketFrame.Text(text, _) =>
                      text
                    }).evalMap(webSocketHub.sendToGame),
                    send = Stream
                      .repeatEval(q.take)
                    /*.merge(
                        Stream
                          .awakeEvery[IO](5.seconds)
                          .map(_ => WebSocketFrame.Text(colorSystemMessage(s"Waiting for players...")))
                          .interruptWhen(Stream.repeatEval(messagesRef.get.map(map => map.size >= 5)))
                      )*/
                  )
                  .onCancel(webSocketHub.disconnectPlayer(playerID))

              } yield w

            case GET -> Root / "start" =>
              for {
                _   <- game.initialize()
                res <- Accepted()
              } yield res

          }
          .orNotFound

      }
    }
  }

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
