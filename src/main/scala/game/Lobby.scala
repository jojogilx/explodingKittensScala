package game

import card.Recipes._
import card.{LightningKittens, Recipes}
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
import org.http4s.circe.CirceEntityCodec.circeEntityEncoder
import org.http4s.circe.toMessageSyntax
import org.http4s.dsl.io._
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.middleware.CORS
import org.http4s.server.websocket.WebSocketBuilder2
import org.http4s.websocket.WebSocketFrame
import players.Player

import java.util.UUID
import scala.concurrent.duration.DurationInt

object Lobby extends IOApp {

  // could be better
  implicit val roomEncoder: Encoder[Game] = (game: Game) =>
    game.gameStateRef.get
      .map(state =>
        Json.obj(
          "name"    -> game.roomName.asJson,
          "players" -> state.players.map({ case Player(playerID, _) => playerID }).asJson,
          "started" -> state.started.asJson,
          "recipe"  -> game.recipe.asJson
        )
      )
      .unsafeRunSync()

  case class RoomCreationForm(roomName: String, playerName: String, recipeName: String)

  implicit val RoomCreateDecoder: Decoder[RoomCreationForm] = deriveDecoder[RoomCreationForm]

  private def httpApp: IO[WebSocketBuilder2[IO] => HttpApp[IO]] = {
    for {
      gamesRef      <- Ref.of[IO, Map[String, Game]](Map.empty)
      roomsQueueRef <- Ref.of[IO, Map[UUID, Queue[IO, WebSocketFrame]]](Map.empty)
      game <- Game.create("room", LightningKittens)
      _ <- gamesRef.updateAndGet(rooms => rooms + ("room" -> game))
    } yield { wsb: WebSocketBuilder2[IO] =>
      {

        val imageService = HttpRoutes
          .of[IO] {

            // curl GET http://127.0.0.1:8080/rooms
            case GET -> Root / "rooms" =>
              for {
                map  <- gamesRef.get
                uuid <- IO.randomUUID
                q    <- Queue.unbounded[IO, WebSocketFrame]
                _    <- roomsQueueRef.update(_ + (uuid -> q))
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

            // websocat ws://127.0.0.1:8080/join/room/player1
            case GET -> Root / "join" / roomName / playerID =>
              for {
                q <- Queue.unbounded[IO, WebSocketFrame]
                res <-
                  gamesRef.get.flatMap(games => {
                    games
                      .get(roomName.trim)
                      .fold(BadRequest(Json.obj("error" -> "Game doesn't exist".asJson)))(game =>
                        game.webSocketHub.connect(playerID,q, game.playerDisconnected(playerID)) *> game.join(playerID.trim).flatMap {
                          case Left(value) => BadRequest("error" -> value.asJson)
                          case Right(_) =>
                            wsb
                              .build(
                                receive = _.filter({
                                  case WebSocketFrame.Text(_) => true
                                  case _ => false
                                }).map({ case WebSocketFrame.Text(text, _) =>
                                  text
                                }).evalMap {
                                  //case "started" => game.initialize()
                                  case "left" => IO.println("player left")
                                  case message => IO.println(s"got $message") *> game.webSocketHub.sendToGame(playerID)(message)
                                },
                                send = Stream
                                  .awakeEvery[IO](30.seconds)
                                  .map(_ => WebSocketFrame.Text("ping")) merge Stream
                                  .repeatEval(q.take)
                              )
                              .onError(err => IO.println(s"error in ws $err"))
                              .onCancel(game.playerDisconnected(playerID.trim))
                        }
                      )
                  })
              } yield res

            // websocat ws://127.0.0.1:8080/start/room1
            case GET -> Root / "start" / roomName =>
              for {
                games <- gamesRef.get.map(_.keys.toList)
                res <-
                  if (!games.contains(roomName.trim)) BadRequest("error" -> "Room doesn't exist".asJson)
                  else
                    gamesRef.get.flatMap(rooms => {
                      rooms
                        .get(roomName.trim)
                        .fold(BadRequest("error" -> "error".asJson))(game =>
                          game.start() *> Ok()
                        )
                    })
              } yield res

            case GET -> Root / "recipes" =>
              val recipes = Recipes.recipesList.asJson.noSpaces
              Ok().map(_.withEntity(recipes))

            case GET -> Root / "ping" =>
              Ok().map(_.withEntity("pong"))

            case req @ POST -> Root / "create" =>
              req.decodeJson[RoomCreationForm].flatMap { form =>
                val roomName     = form.roomName
                val recipeOption = getRecipe(form.recipeName)

                for {
                  games <- gamesRef.get.map(_.keys.toList)
                  res <-
                    if (games.contains(roomName.trim)) BadRequest(Json.obj("error" -> "Game already exists".asJson))
                    else {
                      for {
                        room <- recipeOption.fold(
                          IO
                            .raiseError[Game](new Exception("Recipe not found"))
                        )(recipe => Game.create(roomName.trim, recipe))
                        map <- gamesRef.updateAndGet(rooms => rooms + (roomName.trim -> room))
                        _ <- roomsQueueRef.get.flatMap(m =>
                          m.values.toList.traverse(_.offer(WebSocketFrame.Text(map.values.toList.asJson.noSpaces)))
                        )
                        res <- Ok()
                      } yield res
                    }
                } yield res
              }

            case POST -> Root / "reset" / game =>
              for {
                games <- gamesRef.get.map(_.keys.toList)
                res <-
                  if (!games.contains(game.trim)) BadRequest("error" -> "Room doesn't exist".asJson)
                  else
                    gamesRef.get.flatMap(rooms => {
                      rooms
                        .get(game.trim)
                        .fold(BadRequest("error" -> "error".asJson))(game =>
                          game.reset() *> Ok()
                        )
                    })
              } yield res

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
