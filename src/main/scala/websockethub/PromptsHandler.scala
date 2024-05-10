package websockethub

import card.{CatCard, Defuse, ExplodingKitten, Nope}
import cats.effect.IO
import cats.implicits.catsSyntaxApplicativeId
import players.Player
import players.Player.{Hand, PlayerID}
import websockethub.Event.HandEvent



case class PromptsHandler(webSocketHub: WebSocketHub) {

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
          ) *> chooseBetween2Options(playerID,opt1, opt1Accept, opt2, opt2Accept)
      }

    } yield answer).flatTap(_ => IO.println("here"))



//  def targetAttackPrompter(playerID: PlayerID, playersCanTarget: List[Player]): IO[Player] =
//    chooseWithIndex(
//      webSocketHub.sendToPlayer(playerID)(_),
//      () => webSocketHub.getGameInput(playerID),
//      "\nWho do you want to target?\n",
//      playersCanTarget,
//      (players: List[Player]) => players.map(_.playerID),
//      "\n"
//    )

//  def chooseWithIndex[T](
//      sender: String => IO[Unit],
//      receiver: () => IO[String],
//      prompt: String,
//      list: List[T],
//      display: List[T] => List[String],
//      separator: String
//  ): IO[T] =
//    for {
//      _   <- sender(prompt)
//      _   <- webSocketHub.sendToHost(display(list).toString())
////      _   <- webSocketHub.sendToHost(getStringWithIndex(display(list), separator))
//      input <- receiver().map(_.toIntOption)
//
//      thing <- input match {
//        case Some(index) =>
//          list.lift(index - 1) match {
//            case Some(value) => value.pure[IO]
//            case None =>
//              webSocketHub.sendToHost("Invalid index") *> chooseWithIndex[T](
//                sender,
//                receiver,
//                prompt,
//                list,
//                display,
//                separator
//              )
//          }
//        case None =>
//          webSocketHub.sendToHost(s"Invalid input, choose between 1 and ${list.length}") *> chooseWithIndex[T](
//            sender,
//            receiver,
//            prompt,
//            list,
//            display,
//            separator
//          )
//      }
//    } yield thing


  def playCardsPrompt(player: Player, playerHand: Hand): IO[Option[List[Int]]] =
    for {
      _ <- IO.println("oi")
      _ <- webSocketHub.sendToPlayer2(player.playerID)(HandEvent(playerHand))// switch to a PlayCardRequest
      _ <- IO.println("oi2")
      answer <- webSocketHub.getGameInput(player.playerID)
      _ <- IO.println(s"answer is $answer")
      result <- answer match {
        case "n" => None.pure[IO]
        case s"${c1},${c2}" =>
          {
            (c1.toIntOption, c2.toIntOption) match {
              case (Some(i),Some(j)) if List(i,j).forall(i => {
                playerHand.indices.contains(i) && (playerHand(i) match {
                  case _: CatCard => true
                  case _          => false
                })
              }) =>
                Some(List(i,j)).pure[IO]
              case _ =>
                webSocketHub.sendToPlayer(
                  player.playerID)(
                  "Invalid input, play 2 indices of cat cards (e.g.: 1,2)"
                ) *> playCardsPrompt(player,playerHand)
            }
          } *> None.pure[IO]
        case s"${c1},${c2},${c3}" if c1.toIntOption.isDefined && c2.toIntOption.isDefined && c3.toIntOption.isDefined =>
          {
            List(c1.toInt, c2.toInt, c3.toInt) match {
              case list if list.forall(i => {
                playerHand.indices.contains(i) && (playerHand(i) match {
                  case _: CatCard => true
                  case _          => false
                })
              }) =>
                Some(list).pure[IO]
              case _ =>
                webSocketHub.sendToPlayer(
                  player.playerID)(
                  "Invalid input, play 3 indices of cat cards (e.g.: 1,2,3)"
                ) *> playCardsPrompt(player,playerHand)
            }
          } *> None.pure[IO]
        case x if x.toIntOption.isDefined =>
          x.toInt match {
            case i if playerHand.indices contains i =>
              playerHand(i) match {
                case ExplodingKitten | Defuse | Nope =>
                  webSocketHub.sendToPlayer(
                    player.playerID)(
                   "You can't play this card right now"
                  ) *> playCardsPrompt(player,playerHand)
                case _ => Some(List(i)).pure[IO]
              }

            case _ =>
              webSocketHub.sendToPlayer(player.playerID)( "Invalid index") *> playCardsPrompt(
                player,playerHand
              )
          }

        case _ =>
          webSocketHub.sendToPlayer(player.playerID)("Invalid input") *> playCardsPrompt(
            player,playerHand
          )
      }
      _ <- IO.println("end")
    } yield result


}
