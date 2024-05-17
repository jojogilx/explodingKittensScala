package websockethub

import card.{Card, CatCard, Defuse, ExplodingKitten, Nope}
import cats.effect.IO
import cats.implicits.catsSyntaxApplicativeId
import players.Player.{Hand, PlayerID}
import websockethub.Event._

case class PromptsHandler(webSocketHub: WebSocketHub) {

  def chooseBetween2Options(
      playerID: PlayerID,
      opt1: String,
      opt1Accept: String,
      opt2: String,
      opt2Accept: String
  ): IO[Either[Unit, Unit]] =
    (for {
      _ <- webSocketHub.sendToPlayer(playerID)(
        s"Do you want to $opt1 or $opt2? ($opt1Accept to $opt1, $opt2Accept to $opt2)"
      )

      inp <- webSocketHub.getGameInput(playerID).map(_.trim)

      answer <- inp match {
        case s"${ans}" if ans == opt1Accept => Left().pure[IO]
        case s"${ans}" if ans == opt2Accept => Right().pure[IO]
        case _ =>
          webSocketHub.sendToPlayer(playerID)(
            "Invalid answer. Please type s to spectate or q to quit"
          ) *> chooseBetween2Options(playerID, opt1, opt1Accept, opt2, opt2Accept)
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

  def playCardsPrompt(player: PlayerID, playerHand: Hand): IO[Option[List[Int]]] = {
    for {
      _      <- webSocketHub.sendToPlayer2(player)(CardsInHand(playerHand)) // switch to a PlayCardRequest
      answer <- webSocketHub.getGameInput(player)
      _      <- IO.println(s"answer is $answer")
      result <- answer match {
        case "n" => webSocketHub.broadcast(Information(s"${player} skipped")) *> None.pure[IO]

        case list if !list.split(",").forall(_.toIntOption.isDefined) =>
          webSocketHub.sendToPlayer(player)("Invalid input") *> playCardsPrompt(
            player,
            playerHand
          )

        case ls
          if ls
            .split(",")
            .forall(c =>
              c.toIntOption.fold(false)(index =>
                playerHand.indices.contains(index) && (playerHand(index) match {
                  case _: CatCard => true
                  case _          => false
                })
              )
            ) => Some(ls.split(",").toList.map(c => c.toInt)).pure[IO]


        case i if playerHand.indices contains i.toInt =>
          playerHand(i.toInt) match {
            case ExplodingKitten | Defuse | Nope =>
              webSocketHub.sendToPlayer(player)(
                "You can't play this card right now"
              ) *> playCardsPrompt(player, playerHand)
            case _ => Some(List(i.toInt)).pure[IO]
          }

        case _ =>
          webSocketHub.sendToPlayer(player)(
            "Invalid input, play 1 action card. Or play 2 or 3 indices of cat cards (e.g.: 4,5 or 1,2,3)"
          ) *> playCardsPrompt(player, playerHand)


      }
    } yield result
  }

  def garbageCollectionPrompt(playersWithCards: List[PlayerID]): IO[Unit] = IO.unit
  /*  for {
      _ <- playersWithCards.parTraverse { pID =>
        webSocketHub.sendToPlayer2(pID)(Information("GarbageCollection"))
      }
      ans <- webSocketHub.getPendingInputs(playersWithCards)
      res <- ans.fold(false.pure[IO]) { pID =>
        playCardByID(pID, playerHandsWithNope(pID).indexOf(Nope)) *> webSocketHub.broadcast(
          s"$pID played $Nope"
        ) *> true.pure[IO]
            }

    } yield result
   */

  def choosePlayer(playerID: PlayerID, players: List[PlayerID]): IO[PlayerID] =
    for {
      _      <- webSocketHub.sendToPlayer2(playerID)(TargetPlayer(players))
      string <- webSocketHub.getGameInput(playerID).map(_.toIntOption)
      valid <- string match {
        case Some(value) =>
          value match {
            case x if (1 to players.length) contains x =>
              players.filterNot(_ == playerID)(x - 1).pure[IO]
            case _ =>
              webSocketHub.sendToPlayer(playerID)("Invalid index") *> choosePlayer(playerID, players)
          }
        case None =>
          webSocketHub.sendToPlayer(playerID)("Invalid input") *> choosePlayer(playerID, players)
      }
    } yield valid


  /** Prompt for the alter the future card, players are asked to order the next 3 cards
   * @param cards3
   *   the next 3 cards
   * @param playerID
   *   the current player's id
   * @return
   *   the new order of the cards, chosen by the player
   */
  def alterTheFuture(cards3: List[Card], playerID: PlayerID): IO[String] = {
    for {
      _ <- webSocketHub.sendToPlayer2(playerID)(AlterCardOrder(cards3))
      string <- webSocketHub.getGameInput(playerID)
      valid <- string.toSet match {
        case set if set == Set('1', '2', '3') => string.pure[IO]
        case s =>
          webSocketHub.sendToPlayer(playerID)(
            s"Invalid input $s, please specify order using only numbers"
          ) *> alterTheFuture(cards3, playerID)
      }
    } yield valid
  }
}
