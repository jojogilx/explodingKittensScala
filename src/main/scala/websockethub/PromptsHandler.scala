package websockethub

import card._
import cats.effect.{Deferred, IO}
import cats.implicits.{catsSyntaxApplicativeId, catsSyntaxParallelTraverse1, toFoldableOps, toTraverseOps}
import players.Player.{Hand, PlayerID}
import websockethub.Event._

import scala.concurrent.duration.DurationInt

case class PromptsHandler(webSocketHub: WebSocketHub) {

  def playCardsPrompt(player: PlayerID, playerHand: Hand): IO[Option[List[Int]]] = {
    for {
      _ <- webSocketHub.sendToPlayer(player)(Playing(true))
      answer <- webSocketHub.getGameInput(player)
      _ <- webSocketHub.sendToPlayer(player)(Playing(false))
      result <- answer match {
        case "n" => webSocketHub.broadcast(Information(s"$player passed")) *> None.pure[IO]

        case string => string.split(",").toList.map(_.toIntOption).sequence match {
          case Some(listIndices) => listIndices.map(i => playerHand.lift(i)).sequence match {
            case Some(listCards) => listCards match {
              case card::Nil => card match {
                case ExplodingKitten | Defuse | Nope =>
                webSocketHub.sendToPlayer(player)(
                Error("You can't play this card right now")
                ) *> playCardsPrompt(player, playerHand)
                case _ => Some(listIndices).pure[IO]

              }

              case h::tail if tail.length <= 2 =>
                if(tail.forall(card => card == h || card == FeralCat)) Some(listIndices).pure[IO]
                else webSocketHub.sendToPlayer(player)(
                  Error("Invalid play. Play 1 action card or 2/3 equal cards for combos")
                ) *> playCardsPrompt(player, playerHand)

              case _ =>  webSocketHub.sendToPlayer(player)(
                Error("Invalid play, play 1 action card. Or play 2/3 equal cards for combos")
              ) *> playCardsPrompt(player, playerHand)
            }
            case None => webSocketHub.sendToPlayer(player)(Error("Indices out of hand bounds")) *> playCardsPrompt(
              player,
              playerHand
            )
          }

          case None => webSocketHub.sendToPlayer(player)(Error("Couldn't parse indices to int")) *> playCardsPrompt(
            player,
            playerHand
          )
        }


        case _ =>
          webSocketHub.sendToPlayer(player)(
            Error("Invalid play, play 1 action card. Or play 2 or 3 equal cards for combos")
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
      _      <- webSocketHub.sendToPlayer(playerID)(TargetPlayer(players))
      string <- webSocketHub.getGameInput(playerID).map(_.toIntOption)
      valid <- string match {
        case Some(value) =>
          value match {
            case x if (0 to players.length) contains x =>
              players(x).pure[IO]
            case _ =>
              webSocketHub.sendToPlayer(playerID)(Error("Invalid index")) *> choosePlayer(playerID, players)
          }
        case None =>
          webSocketHub.sendToPlayer(playerID)(Error("Invalid input")) *> choosePlayer(playerID, players)
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
      _      <- webSocketHub.sendToPlayer(playerID)(AlterCardOrder(cards3))
      string <- webSocketHub.getGameInput(playerID)
      valid <- string.toSet match {
        case set if set == Set('0', '1', '2') => string.pure[IO]
        case s =>
          webSocketHub.sendToPlayer(playerID)(
            Error(s"Invalid input $s, please specify order using only numbers")
          ) *> alterTheFuture(cards3, playerID)
      }
    } yield valid
  }

  def chooseCard(playerID: PlayerID, cards: List[Card]): IO[Card] =
    for {
      _      <- webSocketHub.sendToPlayer(playerID)(ChooseCard(cards))
      string <- webSocketHub.getGameInput(playerID).map(_.toIntOption)
      valid <- string match {
        case Some(value) =>
          value match {
            case x if cards.indices contains x =>
              cards(x).pure[IO]
            case _ =>
              webSocketHub.sendToPlayer(playerID)(Error("Invalid index")) *> chooseCard(playerID, cards)
          }
        case None =>
          webSocketHub.sendToPlayer(playerID)(Error("Invalid input")) *> chooseCard(playerID, cards)
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
      _      <- webSocketHub.sendToPlayer(player)(GarbageCollect(hand))
      answer <- webSocketHub.getGameInput(player)
      result <- answer.toIntOption match {
        case Some(i) if hand.indices contains i => (player,i).pure[IO]
        case _ =>
          webSocketHub.sendToPlayer(player)(
            Error("Invalid input")
          ) *> garbageCollectionFrom(player, hand)

      }
      _      <- webSocketHub.sendToPlayer(player)(Information("end2"))
      _ <- IO.println("end")
    } yield result

  /**
   * asks a player if nope action
   * @param playerID - player asked
   * @param hand - their hand
   * @param deferred - deferred controlling the other answers
   * @return
   */
  def getNopeFrom(playerID: PlayerID, hand: Hand, deferred: Deferred[IO, (PlayerID, Int)]): IO[Unit] = {
    IO.race(deferred.get,
      for {
        nope <- webSocketHub.getGameInput(playerID).map(_.toIntOption)
        _ <- nope match {
          case Some(value) =>
            hand.get(value) match {
              case Some(card) if card == Nope => deferred.complete((playerID, value))
              case _ =>
                webSocketHub.sendToPlayer(playerID)(Error("Invalid input")) *> getNopeFrom(playerID, hand, deferred)
            }
          case None =>
            webSocketHub.sendToPlayer(playerID)(Error("Invalid input")) *> getNopeFrom(playerID, hand, deferred)
        }
      } yield ())
  }.void


}
