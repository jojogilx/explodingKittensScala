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
      messagesRef <- Ref.of[IO, Map[String, Map[UUID, Queue[IO, WebSocketFrame]]]](Map.empty)
    } yield { wsb: WebSocketBuilder2[IO] =>
      {

        def joinRoom(playerID: String, roomName: String): IO[Either[String, Room]] = {
          roomsRef.modify(map =>
            map.get(roomName) match {
              case Some(room) if room.nPlayers >= 5 =>
                (map, Left(s"Room ${room.name} is full"))

              case Some(room) =>
                val newRoom = Room.joinRoom(room, Player(playerID, WhiteText))

                (map + (roomName -> newRoom), Right(newRoom))
              case _ => (map, Left(s"Room not found"))
            }
          )
        }

        def broadcastMessage(message: String, roomName: String): IO[Unit] = { // inject this function into game
          messagesRef.get.map(
            _.get(roomName).flatMap(map =>
              map.values.toList.traverse { queue =>
                val a = queue.offer(WebSocketFrame.Text(message))
                a
              }
            )
          )
        }

        ErrorHandling {
          HttpRoutes
            .of[IO] {
              // print current room and list of players
              // command createRoom name > creates room
              // command join roomName > sends get to /room if there is one

              case GET -> Root / "lobby" =>
                for {
                  map <- roomsRef.get
                  _   <- IO.println(map)

                  string = map.values.zipWithIndex.map {
                    case (room, i) => {
                      val color = if (room.started) RedText else GreenText
                      s"$color${i + 1}. ${room.name}, Players(${room.nPlayers}): ${room.players.map(_.playerID).toString()}$ResetText\n"
                    }
                  }.mkString
                  _        <- IO.println(string)
                  response <- Ok(string)

                } yield response

              case POST -> Root / "create" / name =>
                for {
                  room     <- roomsRef.updateAndGet(map => map + (name -> Room(name, 0, started = false, List.empty)))
                  _        <- IO.println(room)
                  response <- Ok(s"Room $name created")
                } yield response

              case POST -> Root / "room" / name / playerName =>
                for {
                  either <- joinRoom(playerName, name)
                  result <- either
                    .fold(string => BadRequest(string), room => broadcastMessage(s"$playerName joined the game", name))
                    .flatMap(_ => Accepted())
                } yield result

              case GET -> Root / "room" / roomName =>
                for {
                  uid <- IO.randomUUID
                  q   <- Queue.unbounded[IO, WebSocketFrame]
                  _   <- messagesRef.update(map => map + (uid -> q))

                  w <- wsb
                    .build(
                      receive = _.void,
                      send = Stream.repeatEval(
                        q.take
                      )
                    )
                    .onCancel(
                      messagesRef.update(map => map - uid) *> IO.println(messagesRef.get.map(_.values.toList.length))
                    )

                } yield w

            }
            .orNotFound

        }
      }
    }
  }

  // Run the lobby server
  override def run(args: List[String]): IO[ExitCode] =
    httpApp.flatMap {
      EmberServerBuilder
        .default[IO]
        .withHost(ipv4"127.0.0.1")
        .withPort(port"8083")
        .withHttpWebSocketApp(_)
        .build
        .useForever
    }
}
