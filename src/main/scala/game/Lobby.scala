package game

import cats.effect._
import cats.effect.std.Queue
import cats.implicits.{catsSyntaxApplicativeId, catsSyntaxOptionId, toFunctorOps}
import com.comcast.ip4s.IpLiteralSyntax
import fs2.{Pipe, Stream}
import org.http4s.{HttpApp, HttpRoutes, Response, Uri, UrlForm}
import org.http4s.dsl.io._
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.headers.Location
import org.http4s.server.middleware.ErrorHandling
import org.http4s.server.websocket.WebSocketBuilder2
import org.http4s.websocket.WebSocketFrame
import players.Player
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
      roomsRef    <- Ref.of[IO, Map[String, Room]](Map.empty)
      messagesRef <- Ref.of[IO, Map[UUID, Queue[IO, WebSocketFrame]]](Map.empty)
    } yield { wsb: WebSocketBuilder2[IO] =>
      ErrorHandling {
        HttpRoutes.of[IO] {
          //print current room and list of players
          // command createRoom name > creates room
          // command join roomName > sends get to /room if there is one

          case GET -> Root / "lobby" =>
            for {
              map <- roomsRef.get
              _ <- IO.println(map)

              string = map.values.zipWithIndex.map {
                case (room, i) => {
                  val color = if(room.started) RedText else GreenText
                  s"$color${i+1}. ${room.name}, Players(${room.nPlayers}): ${room.players.mkString("[",",","]")}$ResetText\n"
                }
              }.mkString
              _ <- IO.println(string)
              response <- Ok(string)

            } yield response



          case POST -> Root / "create" / name =>
            for {
              room <- roomsRef.updateAndGet(map => map + (name -> Room(name, 0, started = false, List.empty)))
              _ <- IO.println(room)
              response <- Ok(s"Room $name created")
            } yield response


          case POST -> Root / "room" / name =>

            roomsRef.get.flatMap { rooms =>
              rooms.get(name) match {
                case Some(room) if room.nPlayers >= 5 =>
                  Forbidden(s"Room $room full")

                case Some(room) => //on failed, remove player
                  for {
                    _ <- IO.print("Enter your name >> ")
                    playerName <- IO.readLine.map(_.trim)
                    player = Player(playerName, WhiteText)
                    _ <- roomsRef.update(rooms => rooms + (room.name -> joinRoom(room,player)))
                    w <- wsb.build(
                      receive = _.void,
                      send = Stream.repeatEval(
                        WebSocketFrame.Text(room.name).pure[IO]
                      )
                    )

                  } yield w


                case None =>
                  NotFound(s"Room $name not found")
              }
            }
        }.orNotFound
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
