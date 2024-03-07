package game

import card.Cards._
import card.Deck
import cats.effect._
import cats.implicits._
import players.Players._

import scala.util.Random

case class Game(nPlayers: Int) {

  private var players: List[Player] = List.empty[Player]
  private var currentPlayerIndex    = 0

  private var drawPile: Deck = Deck(List.empty[Card])

  private var discardPile: Deck = Deck(List.empty[Card])

  // await players join
  // await start game
  // distribute cards
  // manage turns
  // manage win

  private def joinGame(playerID: PlayerID): IO[Unit] = IO.pure {
    players = players :+ Player(playerID)
    println(s"$playerID joined the game.")
  }

  private def initializeDeck(nPlayers: Int): IO[Unit] = IO.pure {
    drawPile = Deck.initShuffledNopeSauce(nPlayers)
  }
  private def setRandomStartingPlayer(): IO[Unit] = IO.pure {
    currentPlayerIndex = Random.nextInt(players.length)

  } *> IO.println(s"${players(currentPlayerIndex).playerID}'s starting")
  private def getPlayersNames: IO[Unit] = {
    (1 to nPlayers).toList.foldLeft(IO.unit) { (acc, i) =>
      acc.flatMap { _ =>
        for {
          _    <- IO.print(s"Insert player $i's name $nPlayers>>")
          name <- IO.readLine.map(_.trim)
          _    <- joinGame(name)
        } yield ()
      }
    }
  }
  def initialize(): IO[Unit] = {
    for {
      _ <- IO.println("initializing...")
      _ <- getPlayersNames
      _ = initializeDeck(nPlayers)
      _ <- handCards()
      _ <- setRandomStartingPlayer()
      _ <- gameLoop(players(currentPlayerIndex))

    } yield ()
  }
  private def nextPlayer(): IO[Player] = IO.pure {
    println(currentPlayerIndex + 1)

    currentPlayerIndex = currentPlayerIndex + 1 match {
      case x if (1 until players.length).contains(x) => x
      case _                                         => 0
    }

    println(currentPlayerIndex + 1)
    players(currentPlayerIndex)
  }
  private def killPlayer(playerIndex: Int): IO[Unit] =
    for {
      _ <- IO.println(s"${players(playerIndex).playerID} died")
      _ <- IO.pure {
        val (left, right) = players.splitAt(playerIndex)
        players = left ::: right.drop(1)
      }
      _ <- IO.println(s"Still playing: ${players.toString()}")
    } yield ()
  private def gameLoop(player: Player): IO[Unit] = {
    val p = for {
      _          <- playerTurn(player)
      nextPlayer <- nextPlayer()
    } yield nextPlayer

    checkWinner().flatMap({
      case Some(player) => IO.println(s"${player.playerID} won the game")
      case None => p.flatMap(nextPlayer => gameLoop(nextPlayer))
    })


  }
  private def playerTurn(player: Player): IO[Unit] = {
    for {
      _ <- IO.println(
        s"\n-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-"
      )
      _ <- IO.println(s"\n${player.playerID}'s turn")
      _ <- IO.println(s"\nYour hand is: \n ${player.handWithIndex()}")

      playOrPass <- askPlayOrPass(player) // Does player want to play a card?
      cardOpt <- playOrPass.fold(Option.empty[Card].pure[IO])(_ => askForCard(player)) // Which card?
      playerSkipped <- cardOpt.fold(false.pure[IO])(card => {
        discardPile = discardPile.prepend(card)
        handleCardPlayed(card)
      }) // If card played, does player skip draw?

      _ <-
        if (!playerSkipped) {
          for {
            card <- drawCard()
            _ <- IO.println(s"${player.playerID} drew a $card") // todo this print only for player
            _ <- card match {
              case ExplodingKitten() =>
                IO.println(s"${player.playerID} drew a Exploding Kitten") *> player.tryGetDefuse.fold(
                  killPlayer(currentPlayerIndex)
                )(_ => IO.println("Defuse used"))
              case card => player.drawCard(card).pure[IO]
            }
          } yield ()
        } else IO.unit

    } yield ()
  }
  private def askPlayOrPass(player: Player): IO[Option[Boolean]] =
    for {
      _      <- IO.println("Do you wish to play a card? (type y/n)")
      answer <- IO.readLine.map(_.trim)
      result <- answer match {
        case "y" => Some(true).pure[IO]
        case "n" => None.pure[IO]
        case _   => IO.println("Answer not valid, please type y or n.") *> askPlayOrPass(player)
      }
    } yield result
  private def askForCard(player: Player): IO[Option[Card]] =
    for {
      _ <- IO.println("\nEnter the index of the card you want to play (c to cancel): ")
      cardOpt <- IO.readLine.map(_.trim).flatMap {
        case "c" => None.pure[IO]
        case x if x.toIntOption.isDefined =>
          x.toInt match {
            case i if (1 to player.hand.length) contains i =>
              Some(player.playCard(i - 1)).pure[IO] // handleCardPlayed(player.playCard(i - 1))

            case _ => IO.println("Invalid index") *> askForCard(player)
          }
        case _ => IO.println("Invalid input") *> askForCard(player)
      }
    } yield cardOpt

  // option next playerID?
  private def handleCardPlayed(card: Card): IO[Boolean] = {
    // todo: dont allow: play a defuse or a nope (on first card)
    card match {
      case Shuffle() =>
        drawPile = drawPile.shuffled
        false.pure[IO]

      case Skip() => true.pure[IO]

      case AlterTheFuture3X() => { // doesnt work
        if (drawPile.length < 3) drawPile = Deck.initFromDiscardPile(drawPile, discardPile)

        val (cards3, deckRemaining) = drawPile.getFirstN(3)

        for {
          _      <- IO.println("Next three cards are: \n") // todo send only for current player
          number <- getCardOrder(cards3, deckRemaining)
        } yield ()
        false.pure[IO]
      }

      case SwapTopAndBottom() =>
        drawPile = drawPile.swapTopAndBottom
        false.pure[IO]

      case Attack2X() =>
        nextPlayer().flatMap(player => playerTurn(player))
        true.pure[IO]

      case TargetedAttack2X() => // todo ask for target
        true.pure[IO]

      case CatomicBomb() =>
        drawPile = drawPile.withBombsOnTop
        true.pure[IO]

      case Bury() =>
        for {
          card <- drawCard()
          _ <- IO.println("Where do you want to bury this card?")
          _ <- buryCard(card)
        } yield true

      case Reverse() =>
        drawPile = drawPile.reversed
        false.pure[IO]

      case Tacocat() | FeralCat() => false.pure[IO]

    }
  }

  private def buryCard(card : Card): IO[Unit] =
    for {

      _ <- IO.println(s"Insert a number between 1 and ${drawPile.length} >> ")
      string <- IO.readLine.map(_.trim.toIntOption)
      _ <- string match {
        case Some(index) if (0 until  drawPile.length).contains(index - 1) =>
          drawPile = drawPile.insertAt(index-1,card)
          ().pure[IO]
        case _ =>
          IO.println("Invalid input ") *> buryCard(card)
      }

    } yield ()


  private def alterTheFuture(cards3: List[Card], remainingDeck: List[Card], order: String): String = {
    println(cards3)
    val cardsnew = order.map(_.toString.toInt - 1).map(cards3).toList

    println(cardsnew)
    drawPile = Deck(cardsnew ++ remainingDeck)
    order
  }

  private def getCardOrder(cards3: List[Card], deckRemaining: List[Card]): IO[String] =
    for {
      _ <- IO.println(s"${cards3.zipWithIndex.foldLeft("") { case (acc, (card, i)) =>
          acc ++ s"${i + 1}. $card\n"
        }}")
      _      <- IO.println("Insert new card order (e.g. 213) >>")
      string <- IO.readLine.map(_.replaceAll(" ", ""))
      valid <- string.toSet match {
        case set if set == Set('1', '2', '3') => alterTheFuture(cards3, deckRemaining, string).pure[IO]
        case _ =>
          IO.println("Invalid input, please specify order using only numbers") *> getCardOrder(cards3, deckRemaining)
      }
    } yield valid

  private def handCards(): IO[Unit] = IO.pure {
    println("handing cards...")

    val (deckWOBombs, bombs) = drawPile.removeBombs(drawPile)
    var deck                 = deckWOBombs

    players.foreach(p => {
      val cards = {
        val (left, right) = deck.splitAt(7)
        deck = right
        left :+ Defuse()
      }
      p.initHand(cards)
    })

    drawPile = Deck(deck ++ bombs).shuffled

  }

  private def switchPiles(): IO[Unit] = IO.pure {
    println("switching piles")

    drawPile = Deck.initFromDiscardPile(discardPile)
    discardPile = Deck(List.empty[Card])
  }

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

  /*  private def drawCard(): IO[Card] = {
    drawPile.draw.fold(switchPiles() *> drawCard())({
      case (deck, card) =>
        drawPile = deck
        card.pure[IO]
    })
  }
   */

  private def checkWinner(): IO[Option[Player]] = IO.pure {
    players.length match {
      case 1 => Some(players.head)
      case _ => None
    }
  }

}

object Main extends IOApp {
  def run(args: List[String]): IO[ExitCode] = {

    for {
      _   <- IO.println("EXPLODING KITTENS - SCALA EDITION\n\n")
      num <- getNumberOfPlayers
      game = Game(num)
      _ <- game.initialize()
    } yield ExitCode.Success

  }

  private def getNumberOfPlayers: IO[Int] =
    for {
      _ <- IO.print("Please insert number of players (2-5) >>")
      num <- IO.readLine.flatMap(_.toIntOption match {
        case Some(x) =>
          x match {
            case i if (2 to 5) contains i => i.pure[IO]

            case _ => IO.println("Invalid number of players") *> getNumberOfPlayers
          }
        case None => IO.println("Invalid input") *> getNumberOfPlayers
      })
    } yield num

}
