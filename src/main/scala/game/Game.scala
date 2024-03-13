package game

import card._
import card.Deck
import card.Deck._
import cats.effect._
import cats.effect.std.Queue
import cats.implicits._
import players.Player
import players.Player._
import utils.TerminalUtils._

import java.util.UUID
import scala.util.Random

case class Game(nPlayers: Int, broadcaster: String => IO[Unit], sendToPlayer: (String, PlayerID) => IO[Unit]) {

  private val PlayerColors: List[String] =
    List(RedText,BlueText,YellowText,GreenText,MagentaText)

  private var players: List[Player] = List.empty[Player]
  private var currentPlayerIndex    = 0

  private var drawPile: Deck = Deck(List.empty[Card])

  private var discardPile: Deck = Deck(List.empty[Card])
  
  // _______________ Game Structure Operations ______________________________//
  def joinGame(playerID: PlayerID, receiveQueue: Queue[IO,String]): IO[Unit] =
    IO.pure { players = players :+ Player(playerID, receiveQueue) } *>
    broadcaster(colorSystemMessage(s"$playerID joined the game")) *> {
      if (players.length == nPlayers)
        initialize()
      else IO.unit
    }

 
  private def setRandomStartingPlayer(): IO[Unit] =
    IO.pure { currentPlayerIndex = Random.nextInt(players.length) } *> broadcaster(s"${players(currentPlayerIndex)}${players(currentPlayerIndex).playerID}$ResetText's starting")

  def initialize(): IO[Unit] = {
    for {
      _ <- broadcaster(gameTitleBanner)
      _ <- broadcaster(colorSystemMessage(s"\r\nInitializing..."))
      _ <- initializeDeck(nPlayers)
      _ <- handCards()
      _ <- setRandomStartingPlayer()
      _ <- gameLoop(players(currentPlayerIndex))

    } yield ()
  }

  private def nextPlayer(): IO[Player] = IO.pure {

    currentPlayerIndex = currentPlayerIndex + 1 match {
      case x if (1 until players.length).contains(x) => x
      case _                                         => 0
    }

    players(currentPlayerIndex)
  }

  private def previousPlayer(): IO[Player] = IO.pure {

    currentPlayerIndex = currentPlayerIndex - 1 match {
      case x if x < 0 => players.length - 1
      case x          => x
    }

    players(currentPlayerIndex)
  }
  private def setNextPlayer(player: Player): IO[Unit] = IO.pure {
    if (players.contains(player))
      currentPlayerIndex = players.indexOf(player)
    else println("Illegal state") //TODO better this, but it shouldn't be possible
  }

  private def killPlayer(playerIndex: Int): IO[Unit] =
    for {
      _ <- broadcaster(s"\n$SkullEmojiUnicode" +
        s" ${players(playerIndex)}${players(playerIndex).playerID}$ResetText died" +
        s" $SkullEmojiUnicode\n")
      _ <- IO.pure {
        val (left, right) = players.splitAt(playerIndex)
        players = left ::: right.drop(1)
      }
    } yield ()

  private def gameLoop(player: Player): IO[Unit] = {
    val p = for {
      _          <- playerTurn(player)
      nextPlayer <- nextPlayer()
    } yield nextPlayer

    checkWinner().flatMap({
      case Some(player) => broadcaster(s"$player${player.playerID} won the game$ResetText")
      case None         => p.flatMap(nextPlayer => gameLoop(nextPlayer))
    })

  }
  private def checkWinner(): IO[Option[Player]] = IO.pure {
    players.length match {
      case 1 => Some(players.head)
      case _ => None
    }
  }

  // ___________ Deck Operations ______________________________//

  private def initializeDeck(nPlayers: Int): IO[Unit] = IO.pure {
    drawPile = Deck.initShuffledNopeSauce(nPlayers)

  }
  private def addCardToDiscardDeck(card: Card): IO[Unit] = {
    discardPile = discardPile.prepend(card)
  }.pure[IO]
  private def drawCard(): IO[Card] = {
    val draw = drawPile.draw

    draw.fold(switchPiles() *> IO({
      drawPile.draw.get match {
        case (deck, card) =>
          drawPile = deck
          card
      }
    }))({ case (deck, card) =>
      drawPile = deck
      card.pure[IO]
    })
  }

  private def handCards(): IO[Unit] = IO.pure {
    broadcaster(colorSystemMessage(s"\nHanding cards...\n"))

    val (deckWOBombs, bombs) = removeDefuseAndBombs(drawPile)
    var deck                 = deckWOBombs

    players.foreach(p => {
      val cards = {
        val (left, right) = deck.splitAt(7)
        deck = right
        left :+ Defuse
      }
      print(s"$p    ")
      println(cards)
      p.initHand(cards)
    })

    drawPile = Deck(deck ++ bombs).shuffled

  }

  private def switchPiles(): IO[Unit] = IO.pure {
    broadcaster(colorSystemMessage(s"switching piles"))

    drawPile = Deck.initShuffledFromDiscardPile(discardPile)
    discardPile = Deck(List.empty[Card])
  }

  private def getFirstNDrawn(n: Int): IO[(List[Card], List[Card])] = {
    if (drawPile.length < n) drawPile = Deck.initShuffledFromDiscardPile2(drawPile, discardPile)

    drawPile.getFirstN(n)
  }.pure[IO]

  // ___________ Turn Operations ______________________________//
  private def playerTurn(player: Player): IO[Unit] = {
    for {
      _ <- broadcaster(
        s"\n-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-"
      )
      _ <- IO.println(s"discard: $discardPile")
      _ <- IO.println(s"draw: $drawPile")
      _ <- broadcaster(s"\n${player}${player.playerID}$ResetText's turn")
      _ <- sendToPlayer(player.playerID, s"\nYour hand is: \n ${player.handWithIndex()}")

      playOrPass <- askPlayOrPass(player) // Does player want to play a card?
      cardOpt <- playOrPass.fold(Option.empty[Card].pure[IO])(_ => askForCard(player)) // Which card?
      playerSkipped <- cardOpt.fold(false.pure[IO])(card => {
        discardPile = discardPile.prepend(card)
        /*askforNope().flatMap(ans =>
          if(!ans) {
            handleCardPlayed(player, card) //dk if this works
          } else {
            false.pure[IO]
          })*/ // todo change when on server
        handleCardPlayed(player, card)

      }) // If card played, does player skip draw?

      _ <-
        if (!playerSkipped) {
          for {
            card <- drawCard()
            _    <- sendToPlayer(player.playerID, s"\n$card drawn")

            _ <- card match {
              case ExplodingKitten =>
                for {
                  _ <- broadcaster(s"\n${player.playerID} drew a $card")
                  _ <- addCardToDiscardDeck(ExplodingKitten)
                  _ <- player.tryGetDefuse.fold(killPlayer(currentPlayerIndex))(defuse =>
                    broadcaster(s"$defuse used") *> addCardToDiscardDeck(defuse)
                  )
                } yield ()
              case card => player.drawCard(card).pure[IO]
            }
          } yield ()
        } else IO.unit

    } yield ()
  }

  private def askPlayOrPass(player: Player): IO[Option[Boolean]] =
    for {
      _      <- sendToPlayer(player.playerID, s"\n${player.playerID}, do you wish to play a card? (y/n)")
     // answer <- IO.readLine.map(_.trim.toLowerCase)
      answer <- player.receiveQueue.take.map(_.trim.toLowerCase)
      result <- answer match {
        case "y" => Some(true).pure[IO]
        case "n" => None.pure[IO]
        case _   => sendToPlayer(player.playerID, s"${RedText}Invalid input$ResetText") *> askPlayOrPass(player)
      }
    } yield result

  private def askForCard(player: Player): IO[Option[Card]] =
    for {
      _ <- sendToPlayer(player.playerID, "\nEnter the index of the card you want to play (c to cancel).")
      //cardOpt <- IO.readLine.map(_.trim.toLowerCase).flatMap {
      cardOpt <- player.receiveQueue.take.map(_.trim.toLowerCase).flatMap {
        case "c" => None.pure[IO]
        case x if x.toIntOption.isDefined =>
          x.toInt match {
            case i if (1 to player.hand.length) contains i =>
              player.hand(i - 1) match {
                case ExplodingKitten | Defuse | Nope =>
                  sendToPlayer(player.playerID, s"${RedText}You can't play this card right now$ResetText") *> askForCard(player)
                case _ => Some(player.playCard(i - 1)).pure[IO]
              }

            case _ => sendToPlayer(player.playerID, s"${RedText}Invalid index$ResetText") *> askForCard(player)
          }
        case _ => sendToPlayer(player.playerID, s"${RedText}Invalid input$ResetText") *> askForCard(player)
      }
    } yield cardOpt

  // option next playerID?
  private def handleCardPlayed(player: Player, card: Card): IO[Boolean] = {
    card match {
      case Shuffle =>
        drawPile = drawPile.shuffled
        false.pure[IO]

      case Skip => true.pure[IO]

      case AlterTheFuture3X =>
        for {
          res <- getFirstNDrawn(3)
          (cards3, deckRemaining) = res
          number <- getCardOrder(player, cards3, deckRemaining)
          _      <- alterTheFuture(cards3, deckRemaining, number)
        } yield false

      case SwapTopAndBottom =>
        drawPile = drawPile.swapTopAndBottom
        false.pure[IO]

      case Attack2X =>
        for {
          nextPlayer <- nextPlayer()
          _          <- playerTurn(nextPlayer)
          _          <- previousPlayer()
        } yield true

      case TargetedAttack2X =>
        for {
          nextPlayer <- targetAttack(player)
          _          <- setNextPlayer(nextPlayer)
          _          <- playerTurn(nextPlayer)
          _          <- previousPlayer()
        } yield true

      case CatomicBomb =>
        drawPile = drawPile.withExplodingKittensOnTop
        true.pure[IO]

      case Bury =>
        for {
          card <- drawCard()
          _    <- buryCard(player, card)
        } yield true

      case Reverse =>
        drawPile = drawPile.reversed
        false.pure[IO]

      case Tacocat | FeralCat => false.pure[IO]

    }
  }

  // ______________ Card Actions ____________________________//

  private def targetAttack(player: Player): IO[Player] =
    for {
      _ <- sendToPlayer(player.playerID, "\nWho do you want to target? \n")
      _ <- sendToPlayer(player.playerID,
        s"${players.filterNot(_.playerID == player.playerID).zipWithIndex.foldLeft("") { case (acc, (p, i)) =>
            acc ++ s"${i + 1}. ${p.playerID}\n"
          }}"
      )
      _      <- sendToPlayer(player.playerID, "Insert index >> ")
      //string <- IO.readLine.map(_.toIntOption)
      string <- player.receiveQueue.take.map(_.toIntOption)
      valid <- string match {
        case Some(value) =>
          value match {
            case x if (1 until players.length) contains x =>
              players.filterNot(_.playerID == player.playerID)(x - 1).pure[IO]
            case _ => sendToPlayer(player.playerID, s"${RedText}Invalid index$ResetText") *> targetAttack(player)
          }
        case None => sendToPlayer(player.playerID, s"${RedText}Invalid input$ResetText") *> targetAttack(player)
      }
    } yield valid

  private def askforNope(): IO[Boolean] = { // todo: Nope only functional when multiplayer
    /*    for {
          player <- players
          hand = player.hand
          if hand.contains(Nope())
          _ <- printlnForPlayer(player, s"${player.playerID}, do you wish to nope this action? (y/n)")
          res <- askPlayOrPass(player)
          bool <- res.fold(false.pure[IO])(answer => answer.pure[IO])
        } yield bool*/

    true.pure[IO]
  }

  private def buryCard(player: Player, card: Card): IO[Unit] =
    for {
      _      <- sendToPlayer(player.playerID, "Where do you want to bury this card?")
      _      <- sendToPlayer(player.playerID, s"Insert a number between 1 and ${drawPile.length} >> ")
      //string <- IO.readLine.map(_.trim.toIntOption)
      string <- player.receiveQueue.take.map(_.trim.toIntOption)
      _ <- string match {
        case Some(index) if (0 until drawPile.length).contains(index - 1) =>
          drawPile = drawPile.insertAt(index - 1, card)
          ().pure[IO]
        case _ =>
          sendToPlayer(player.playerID, s"${RedText}Invalid input$ResetText") *> buryCard(player, card)
      }

    } yield ()

  private def alterTheFuture(cards3: List[Card], remainingDeck: List[Card], order: String): IO[Unit] = {
    println(cards3)
    val cardsNew = order.map(_.toString.toInt - 1).map(cards3).toList

    println(cardsNew)
    drawPile = Deck(cardsNew ++ remainingDeck)
  }.pure[IO]

  private def getCardOrder(player: Player, cards3: List[Card], deckRemaining: List[Card]): IO[String] =
    for {
      _ <-sendToPlayer(player.playerID, "Next three cards are: \n")
      _ <- sendToPlayer(player.playerID,
        s"${cards3.zipWithIndex.foldLeft("") { case (acc, (card, i)) =>
            acc ++ s"${i + 1}. $card\n"
          }}"
      )
      _      <- sendToPlayer(player.playerID, "Insert new card order (e.g. 213) >>")
      //string <- IO.readLine.map(_.replaceAll(" ", ""))
      string <- player.receiveQueue.take.map(_.replaceAll(" ", ""))
      valid <- string.toSet match {
        case set if set == Set('1', '2', '3') => string.pure[IO]
        case _ =>
          sendToPlayer(player.playerID, s"${RedText}Invalid input, please specify order using only numbers$ResetText") *> getCardOrder(
            player,
            cards3,
            deckRemaining
          )
      }
    } yield valid

}

