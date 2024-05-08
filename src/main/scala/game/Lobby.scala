package game

import card.Recipes
import card.Recipes._
import cats.effect._
import cats.effect.std.Queue
import cats.effect.unsafe.implicits.global
import cats.implicits.{toFunctorOps, toTraverseOps}
import com.comcast.ip4s.IpLiteralSyntax
import io.circe._
import io.circe.generic.semiauto._
import io.circe.syntax._
import fs2._
import org.http4s._
import org.http4s.circe.toMessageSyntax
import org.http4s.dsl.io._
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.middleware.CORS
import org.http4s.server.websocket.WebSocketBuilder2
import org.http4s.websocket.WebSocketFrame
import rooms.Room
import websockethub.Event._

import java.util.UUID
import scala.concurrent.duration.DurationInt

object Lobby extends IOApp {

  // could be better
  implicit val roomEncoder: Encoder[List[Room]] = (l: List[Room]) =>
    {
      l.map(r =>
        r.stateRef.get
          .map(state =>
            Json.obj(
              "name"    -> r.name.asJson,
              "players" -> state.players.asJson,
              "started" -> state.started.asJson,
              "recipe" -> state.recipe.asJson
            )
          )
          .unsafeRunSync()
      )
    }.asJson


  case class RoomCreationForm(roomName: String, playerName: String, recipeName: String)

  implicit val RoomCreateDecoder = deriveDecoder[RoomCreationForm]


  private def httpApp: IO[WebSocketBuilder2[IO] => HttpApp[IO]] = {
    for {
      roomsRef   <- Ref.of[IO, Map[String, Room]](Map.empty)
      roomsQueueRef <- Ref.of[IO, Map[UUID, Queue[IO, WebSocketFrame]]](Map.empty)
    } yield { wsb: WebSocketBuilder2[IO] =>
      {

        val imageService = HttpRoutes
          .of[IO] {

            // curl GET http://127.0.0.1:8080/rooms
            case GET -> Root / "rooms" =>
              for {
                map <- roomsRef.get
                uuid <- IO.randomUUID
                q <- Queue.unbounded[IO, WebSocketFrame]
                _ <- roomsQueueRef.update(_ + (uuid -> q))
                _ <- roomsQueueRef.get.flatMap(queues =>
                  queues.values.toList.traverse { queue =>
                    queue.offer(WebSocketFrame.Text(map.values.toList.asJson.noSpaces))
                  }.void
                )
                w <- wsb.build(
                  receive = _.void,
                  send = Stream
                    .repeatEval(
                      q.take
                    )
                )
              } yield w


            /*
            curl -X POST \
            -d 'name=room1' \
            http://127.0.0.1:8080/create
             */

            // maybe initialize game on create room
            case req @ POST -> Root / "create" =>
               req.decodeJson[RoomCreationForm].flatMap { form =>
                val roomName = form.roomName
                val recipeOption = getRecipe(form.recipeName)

                for {
                  rooms <- roomsRef.get.map(_.keys.toList)
                  res <-
                    if (rooms.contains(roomName.trim)) IO.println("exists") *> BadRequest("Room already exists")
                    else {
                      for {
                        room <- recipeOption.fold(IO.println(s"no recipe? $recipeOption") *> IO.raiseError[Room](new Exception("Recipe not found")))(recipe => Room.create(roomName.trim, recipe))
                        map  <- roomsRef.updateAndGet(rooms => rooms + (roomName.trim -> room))
                        _    <- roomsQueueRef.get.flatMap(m => m.values.toList.traverse(_.offer(WebSocketFrame.Text(map.values.toList.asJson.noSpaces))))
                        res  <- Ok()
                      } yield res
                    }
                } yield res
              }

            // delete room

            // websocat ws://127.0.0.1:8080/join/room1/player1
            case GET -> Root / "join" / roomName / playerID =>
                for {
                  rooms <- roomsRef.get.map(_.keys.toList)
                  q <- Queue.unbounded[IO, WebSocketFrame]
                  res <-
                    if (!rooms.contains(roomName.trim)) BadRequest("Room doesn't exist")
                    else
                      roomsRef.get.flatMap(rooms => {
                        rooms
                          .get(roomName.trim)
                          .fold(BadRequest("error"))(room =>
                            room.join(playerID.trim, q).flatMap {
                              case Left(value) => BadRequest(value)
                              case Right(_) => // this doesn't work because
                                wsb
                                  .build(
                                    receive = _.filter({
                                      case WebSocketFrame.Text(_) => true
                                      case _ => false
                                    }).map({ case WebSocketFrame.Text(text, _) =>
                                      text
                                    }).evalMap(room.sendToGame(playerID.trim)),
                                    send = Stream
                                      .repeatEval(q.take)
                                  )
                                  .onCancel(room.leave(playerID.trim))
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
                            case Right(value) => room.webSocketHub.broadcast(Started()) *> Accepted(value)
                          }
                        )
                    })
              } yield res


            case GET -> Root / "recipes" =>
              val recipes = Recipes.recipesList.asJson.noSpaces
              Ok().map(_.withEntity(recipes))

          }
          .orNotFound

        CORS.policy
          .withAllowOriginAll(imageService)
      }
    }
  }

  override def run(args: List[String]): IO[ExitCode] =
    httpApp.flatMap {
      EmberServerBuilder
        .default[IO]
        .withHost(ipv4"127.0.0.1")
        .withPort(port"8080")
        .withIdleTimeout(5.minutes)
        .withHttpWebSocketApp(_)
        .build
        .useForever
    }
}
