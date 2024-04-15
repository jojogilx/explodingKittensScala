package websockethub

import cats.effect.IO
import cats.implicits.catsSyntaxApplicativeId
import players.Player.PlayerID

case class PromptsHandler(webSocketHub: WebSocketHub) {

  /**
   *
   * @param playerID
   * @return Left if spectating, Right if leaving
   */
  def playOrSpectate(playerID: PlayerID): IO[Either[Unit,Unit]] =
    chooseBetween2Options(playerID, "play", "p", "spectate", "s")


  /**
   *
   * @param playerID
   * @return Left if spectating, Right if leaving
   */
  def spectateOrLeave(playerID: PlayerID): IO[Either[Unit, Unit]] =
    chooseBetween2Options(playerID, "spectate", "s", "quit", "q")




  def chooseBetween2Options(playerID: PlayerID, opt1: String, opt1Accept: String, opt2: String, opt2Accept: String): IO[Either[Unit,Unit]] =
    IO.println("yo") *> (for {
      _ <-IO.println("a")
    _ <- webSocketHub.sendToPlayer(playerID, s"Do you want to $opt1 or $opt2? ($opt1Accept to $opt1, $opt2Accept to $opt2)")
      _ <-IO.println("b")

    inp <- webSocketHub.getGameInput(playerID).map(_.trim)
      _ <-IO.println("c")

    answer <- inp match {
      case s"${ans}" if ans==opt1Accept => Left().pure[IO]
      case s"${ans}"if ans==opt2Accept => Right().pure[IO]
      case _ => webSocketHub.sendToPlayer(playerID, "Invalid answer. Please type s to spectate or q to quit") *> playOrSpectate(playerID)
    }

  } yield answer).flatTap(_ => IO.println("here"))

}