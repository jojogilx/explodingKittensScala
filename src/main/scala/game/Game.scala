package game

import card.Cards._
import card.Deck
import cats.effect._
import cats.implicits.{catsSyntaxApplicativeId, toTraverseOps}
import players.Players._

import scala.util.Random

case class Game(nPlayers: Int) {

  private var players: List[Player] = List.empty[Player]
  private var currentPlayerIndex    = 0

  private var drawPile: Deck = Deck(List.empty[Card])

  private var discardPile: Deck = Deck(List.empty[Card])

  private var skip     = false
  private var attack2X = false

  // await players join
  // await start game
  // distribute cards
  // manage turns
  // manage win

  def joinGame(playerID: PlayerID): IO[Unit] = IO.pure {
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
    (1 to  nPlayers).toList.foldLeft(IO.unit) { (acc, i) =>
      acc.flatMap { _ =>
        for {
          _ <- IO.print(s"Insert player $i's name $nPlayers>>")
          name <- IO.readLine.map(_.trim)
          _ <- joinGame(name)
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
    currentPlayerIndex = (currentPlayerIndex + 1) % players.length
    players(currentPlayerIndex)
  }

  private def removePlayerAt(i: Int): IO[Unit] = IO.pure({
    val (left, right) = players.splitAt(i)
    players = left ::: right.drop(1)
  })


  private def killPlayer(playerIndex: Int): IO[Unit] =
  for {
    _ <- IO.println(s"${players(playerIndex).playerID} died")
    _ <- removePlayerAt(playerIndex)
    _ <- IO.println(s"Still playing: ${players.toString()}")
  } yield ()

  private def gameLoop(player: Player): IO[Unit] = {
    val p = for {
      _ <- IO.println(
        s"\n-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-"
      )
      _          <- IO.println(s"\n${player.playerID}'s turn")
      _          <- IO.println(s"\nYour hand is: \n ${player.handWithIndex()}")
      _          <- askPlayOrPass(player)
      card <- drawCard()
      _ = discardPile.prepend(card)
      _ <- IO.println(s"${player.playerID} drew a $card") //todo this print only for player
      _ <- card match {
        case ExplodingKitten() =>
          IO.println(s"${player.playerID} drew a Exploding Kitten"
          )*> player.tryGetDefuse.fold(killPlayer(currentPlayerIndex))(_ => IO.println("Defuse used"))
        case card => player.drawCard(card).pure[IO]
      }
      nextPlayer <- nextPlayer()
    } yield nextPlayer

    p.flatMap(nextPlayer => gameLoop(nextPlayer))

  }

  private def askPlayOrPass(player: Player): IO[Unit] =
    for {
      _      <- IO.println("Do you wish to play a card? (type y/n)")
      answer <- IO.readLine.map(_.trim)
      result <- answer match {
        case "y" => askForCard(player)
        case "n" => ().pure[IO]
        case _   => IO.println("Answer not valid, please type y or n.") *> askPlayOrPass(player)
      }
    } yield result

  private def askForCard(player: Player): IO[Unit] =
    for {
      _     <- IO.println("\nEnter the index of the card you want to play (c to cancel): ")
      cardI <- IO.readLine.map(_.trim)
      _ <- cardI match {
        case "c" => ().pure[IO]
        case x if x.toIntOption.isDefined =>
          x.toInt match {
            case i if (1 to player.hand.length) contains i => handleCardPlayed(player.playCard(i - 1))

            case _ => IO.println("Invalid index") *> askForCard(player)
          }
        case _ => IO.println("Invalid input") *> askForCard(player)
      }

    } yield ()

  // option next playerID?
  private def handleCardPlayed(card: Card): IO[Unit] = IO.pure {
    card match {
      case Shuffle() =>
        drawPile = drawPile.shuffled

      case Nope() => ???

      case Skip() => skip = true

      case AlterTheFuture3X() => ???

      case SwapTopAndBottom() =>
        drawPile = drawPile.swapTopAndBottom

      case Attack2X() =>
        attack2X = true
        skip = true

      case TargetedAttack2X() => // todo ask for target
        attack2X = true
        skip = true
      case CatomicBomb() => ???
      case Bury()        => ???
      case Reverse()     => drawPile = drawPile.reversed
    }
  }

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
    }))({
      case (deck, card) =>
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
      _ <-IO.println("EXPLODING KITTENS - SCALA EDITION\n\n")
      num <- getNumberOfPlayers
      game = Game(num)
      _ <- game.initialize()
    } yield ExitCode.Success

  }

  private def getNumberOfPlayers: IO[Int] =
    for {
      _ <- IO.print("Please insert number of players (2-5) >>")
      num <- IO.readLine.flatMap(_.toIntOption match {
        case Some(x) => x match {
          case i if (2 to 5) contains i => i.pure[IO]

          case _ => IO.println("Invalid number of players") *> getNumberOfPlayers
        }
        case None => IO.println("Invalid input") *> getNumberOfPlayers
      })
    } yield num

}
