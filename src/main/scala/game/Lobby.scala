package game

import cats.effect._
import cats.effect.std.Queue
import cats.implicits.{toFoldableOps, toTraverseOps}
import com.comcast.ip4s.IpLiteralSyntax
import fs2._
import org.http4s._
import org.http4s.dsl.io.{BadRequest, _}
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.websocket.WebSocketBuilder2
import org.http4s.websocket.WebSocketFrame
import rooms.Room
import utils.TerminalUtils._
import websockethub.WebSocketHub

import scala.concurrent.duration.DurationInt

object Lobby extends IOApp {
  // (max rooms)
  // create room
  // join room
  private def httpApp: IO[WebSocketBuilder2[IO] => HttpApp[IO]] = {
    for {
      // webSocketHub <- WebSocketHub.of
      //   game         <- Game.create(5, webSocketHub)
      roomsRef <- Ref.of[IO, Map[String, Room]](Map.empty)
    } yield { wsb: WebSocketBuilder2[IO] =>
      {

        HttpRoutes
          .of[IO] {

            case GET -> Root / "lobby" =>
              for {
                map <- roomsRef.get
                _   <- IO.println(map)
                strings <- map.values.toList.zipWithIndex.foldLeft(IO.pure("")) { case (accIO, (room, i)) =>
                  for {
                    acc <- accIO
                    roomString <- room.getString
                  } yield acc + s"${i + 1}.  $roomString\n"
                }
                response <- Ok(strings)
              } yield response


            case GET -> Root / "create" / roomName / nPlayers =>
              for {
                rooms <- roomsRef.get.map(_.keys.toList)
                res <-
                  if (rooms.contains(roomName.trim)) BadRequest("Room already exists")
                  else {
                    nPlayers.toIntOption match {
                      case Some(value) if 2 to 5 contains value =>
                        for {
                          room <- Room.create(value, roomName.trim)
                          _    <- roomsRef.update(rooms => rooms + (roomName.trim -> room))
                          res  <- Accepted("Room created")
                        } yield res

                      case _ => BadRequest("Number of players invalid (2-5 players only)")
                    }

                  }

              } yield res

            case GET -> Root / "join" / roomName / playerID =>
              for {
                rooms <- roomsRef.get.map(_.keys.toList)
                q     <- Queue.bounded[IO, WebSocketFrame.Text](1)
                res <-
                  if (!rooms.contains(roomName.trim)) BadRequest("Room doesn't exist")
                  else
                    roomsRef.get.flatMap(rooms => {
                      rooms
                        .get(roomName.trim)
                        .fold(BadRequest("error"))(room =>
                          room.join(playerID.trim, q).flatMap {
                            case Left(value) => BadRequest(value)
                            case Right(_) =>
                              wsb
                                .withOnClose(room.leave(playerID.trim))
                                .build(
                                  receive = _.filter({
                                    case WebSocketFrame.Text(_) => true
                                    case _                      => false
                                  }).map({ case WebSocketFrame.Text(text, _) =>
                                    text
                                  }).evalMap(room.sendToGame),
                                  send = Stream
                                    .repeatEval(q.take)
                                )
                          }
                        )
                    })
              } yield res

            case GET -> Root / "start" / roomName =>
              for {
                rooms <- roomsRef.get.map(_.keys.toList)
                res <-
                  if (!rooms.contains(roomName.trim)) BadRequest("Room doesn't exist")
                  else
                    roomsRef.get.flatMap(rooms => {
                      rooms
                        .get(roomName.trim)
                        .fold(BadRequest("error"))(room =>
                          room.startGame.flatMap {
                            case Left(value)  => BadRequest(value)
                            case Right(value) => Accepted(value)
                          }
                        )
                    })
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
