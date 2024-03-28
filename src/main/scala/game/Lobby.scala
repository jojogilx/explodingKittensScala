package game

import cats.effect._
import cats.effect.std.Queue
import com.comcast.ip4s.IpLiteralSyntax
import fs2._
import org.http4s._
import org.http4s.dsl.io.{BadRequest, _}
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.websocket.WebSocketBuilder2
import org.http4s.websocket.WebSocketFrame
import rooms.Room

import scala.concurrent.duration.DurationInt


object Lobby extends IOApp {
  private def httpApp: IO[WebSocketBuilder2[IO] => HttpApp[IO]] = {
    for {
      roomsRef <- Ref.of[IO, Map[String, Room]](Map.empty)
    } yield { wsb: WebSocketBuilder2[IO] =>
      {

        HttpRoutes
          .of[IO] {

            //curl GET http://127.0.0.1:8080/lobby
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


            // curl GET http://127.0.0.1:8080/create/room1/2
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

            // websocat ws://127.0.0.1:8080/join/room1/player1
            case GET -> Root / "join" / roomName / playerID =>
              for {
                rooms <- roomsRef.get.map(_.keys.toList)
                q     <- Queue.unbounded[IO, WebSocketFrame]
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
                                .build(
                                  receive = _.filter({
                                    case WebSocketFrame.Text(_) => true
                                    case _                      => false
                                  }).map({ case WebSocketFrame.Text(text, _) =>
                                    text
                                  }).evalMap(room.sendToGame(playerID.trim)),
                                  send = Stream
                                    .repeatEval(q.take)

                                ).onCancel(room.leave(playerID.trim))
                          }
                        )
                    })
              } yield res

            // websocat ws://127.0.0.1:8080/start/room1
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
        .withPort(port"8025")
        .withIdleTimeout(5.minutes)
        .withHttpWebSocketApp(_)
        .build
        .useForever
    }
}
