package websockethub

import card.{Card, CatCard, Defuse, ExplodingKitten, Nope}
import cats.effect.IO
import cats.implicits.catsSyntaxApplicativeId
import players.Player.{Hand, PlayerID}
import websockethub.Event._

import scala.concurrent.duration.DurationInt

case class PromptsHandler(webSocketHub: WebSocketHub) {

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
            case x if (0 to players.length) contains x =>
              players(x).pure[IO]
            case _ =>
              webSocketHub.sendToPlayer2(playerID)(Error("Invalid index")) *> choosePlayer(playerID, players)
          }
        case None =>
          webSocketHub.sendToPlayer2(playerID)(Error("Invalid input")) *> choosePlayer(playerID, players)
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


  def chooseCard(playerID: PlayerID, cards: List[Card]): IO[Card] =
    for {
      _      <- webSocketHub.sendToPlayer2(playerID)(ChooseCard(cards))
      string <- webSocketHub.getGameInput(playerID).map(_.toIntOption)
      valid <- string match {
        case Some(value) =>
          value match {
            case x if (0 until cards.length) contains x =>
              cards(x).pure[IO]
            case _ =>
              webSocketHub.sendToPlayer2(playerID)(Error("Invalid index")) *> chooseCard(playerID, cards)
          }
        case None =>
          webSocketHub.sendToPlayer2(playerID)(Error("Invalid input")) *> chooseCard(playerID, cards)
      }
    } yield valid



  def broadCastCountDown(counter: Int): IO[Unit] =
    if (counter <= 0) IO.unit
    else
      for {
        _ <- webSocketHub.broadcast(s"$counter") //remove
        _ <- IO.sleep(1.seconds)
        _ <- broadCastCountDown(counter - 1)
      } yield ()



}
