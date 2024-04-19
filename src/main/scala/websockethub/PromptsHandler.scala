package websockethub

import card.Recipe
import card.Recipes.recipesList
import cats.effect.IO
import cats.implicits.catsSyntaxApplicativeId
import players.Player
import players.Player.PlayerID
import utils.TerminalUtils.{colorErrorMessage, getStringWithIndex}

import scala.util.{Failure, Success, Try}


case class PromptsHandler(webSocketHub: WebSocketHub) {

  /** @param playerID
    * @return
    *   Left if spectating, Right if leaving
    */
  def playOrSpectate(playerID: PlayerID): IO[Either[Unit, Unit]] =
    chooseBetween2Options(playerID, "play", "p", "spectate", "s")

  /** @param playerID
    * @return
    *   Left if spectating, Right if leaving
    */
  def spectateOrLeave(playerID: PlayerID): IO[Either[Unit, Unit]] =
    chooseBetween2Options(playerID, "spectate", "s", "quit", "q")

  def chooseBetween2Options(
      playerID: PlayerID,
      opt1: String,
      opt1Accept: String,
      opt2: String,
      opt2Accept: String
  ): IO[Either[Unit, Unit]] =
    IO.println("yo") *> (for {
      _ <- IO.println("a")
      _ <- webSocketHub.sendToPlayer(playerID)(
        s"Do you want to $opt1 or $opt2? ($opt1Accept to $opt1, $opt2Accept to $opt2)"
      )
      _ <- IO.println("b")

      inp <- webSocketHub.getGameInput(playerID).map(_.trim)
      _   <- IO.println("c")

      answer <- inp match {
        case s"${ans}" if ans == opt1Accept => Left().pure[IO]
        case s"${ans}" if ans == opt2Accept => Right().pure[IO]
        case _ =>
          webSocketHub.sendToPlayer(playerID)(
            "Invalid answer. Please type s to spectate or q to quit"
          ) *> playOrSpectate(playerID)
      }

    } yield answer).flatTap(_ => IO.println("here"))

  def recipePrompt(): IO[Recipe] =
    chooseWithIndex(
      webSocketHub.sendToHost,
      webSocketHub.receiveFromHost,
      "Chose recipe",
      recipesList,
      (recipes: List[Recipe]) => recipes.map(_.toString),
      "\n"
    )

  def targetAttackPrompter(playerID: PlayerID, playersCanTarget: List[Player]): IO[Player] =
    chooseWithIndex(
      webSocketHub.sendToPlayer(playerID)(_),
      () => webSocketHub.getGameInput(playerID),
      "\nWho do you want to target?\n",
      playersCanTarget,
      (players: List[Player]) => players.map(_.playerID),
      "\n"
    )

  def chooseWithIndex[T](
      sender: String => IO[Unit],
      receiver: () => IO[String],
      prompt: String,
      list: List[T],
      display: List[T] => List[String],
      separator: String
  ): IO[T] =
    for {
      _   <- sender(prompt)
      _   <- webSocketHub.sendToHost(getStringWithIndex(display(list), separator))
      input <- receiver().map(_.toIntOption)

      thing <- input match {
        case Some(index) =>
          list.lift(index - 1) match {
            case Some(value) => value.pure[IO]
            case None =>
              webSocketHub.sendToHost(colorErrorMessage("Invalid index")) *> chooseWithIndex[T](
                sender,
                receiver,
                prompt,
                list,
                display,
                separator
              )
          }
        case None =>
          webSocketHub.sendToHost(colorErrorMessage(s"Invalid input, choose between 1 and ${list.length}")) *> chooseWithIndex[T](
            sender,
            receiver,
            prompt,
            list,
            display,
            separator
          )
      }
    } yield thing

}
