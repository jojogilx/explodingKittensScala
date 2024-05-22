package websockethub

import card.{Card, CatCard, Defuse, ExplodingKitten, Nope}
import cats.effect.IO
import cats.implicits.{catsSyntaxApplicativeId, catsSyntaxParallelTraverse1}
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
        case "n" => webSocketHub.broadcast(Information(s"$player skipped")) *> None.pure[IO]

        case list if !list.split(",").forall(_.toIntOption.isDefined) =>
          webSocketHub.sendToPlayer2(player)(Error("Invalid input")) *> playCardsPrompt(
            player,
            playerHand
          )
//this makes possible that player plays 3 different cat cards (not supposed)
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
              ) =>
          Some(ls.split(",").toList.map(c => c.toInt)).pure[IO]

        case i if playerHand.indices contains i.toInt =>
          playerHand(i.toInt) match {
            case ExplodingKitten | Defuse | Nope =>
              webSocketHub.sendToPlayer2(player)(
                Information("You can't play this card right now")
              ) *> playCardsPrompt(player, playerHand)
            case _ => Some(List(i.toInt)).pure[IO]
          }

        case _ =>
          webSocketHub.sendToPlayer2(player)(
            Error("Invalid input, play 1 action card. Or play 2 or 3 indices of cat cards (e.g.: 4,5 or 1,2,3)")
          ) *> playCardsPrompt(player, playerHand)

      }
    } yield result
  }

  def garbageCollectionPrompt(playersWithCards: Map[PlayerID, Hand]): IO[List[(PlayerID, Int)]] =
    playersWithCards.toList.parTraverse { case (pID, hand) =>
      garbageCollectionFrom(pID, hand)
    }

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
      _      <- webSocketHub.sendToPlayer2(playerID)(AlterCardOrder(cards3))
      string <- webSocketHub.getGameInput(playerID)
      valid <- string.toSet match {
        case set if set == Set('0', '1', '2') => string.pure[IO]
        case s =>
          webSocketHub.sendToPlayer2(playerID)(
            Error(s"Invalid input $s, please specify order using only numbers")
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
            case x if cards.indices contains x =>
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
        _ <- webSocketHub.broadcast(Information(s"$counter")) // remove
        _ <- IO.sleep(1.seconds)
        _ <- broadCastCountDown(counter - 1)
      } yield ()

  private def garbageCollectionFrom(
      player: PlayerID,
      hand: Hand
  ): IO[(PlayerID,Int)] =
    for {
      _      <- webSocketHub.sendToPlayer2(player)(GarbageCollect(hand))
      answer <- webSocketHub.getGameInput(player)
      result <- answer.toIntOption match {
        case Some(i) if hand.indices contains i => (player,i).pure[IO]
        case _ =>
          webSocketHub.sendToPlayer2(player)(
            Error("Invalid input")
          ) *> garbageCollectionFrom(player, hand)

      }
    } yield result

}
