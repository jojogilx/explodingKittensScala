package game

import card._
import card.Deck._
import cats.effect.{IO, Ref}
import cats.implicits._
import gamestate._
import players.Player
import players.Player.{Hand, PlayerID}
import utils.TerminalUtils._
import websockethub.WebSocketHub

import scala.util.Random

case class Game(
    nPlayers: Int,
    webSocketHub: WebSocketHub,
    gameStateRef: Ref[IO, State]
) {
  def joinGame(player: PlayerID): IO[Unit] = {
    gameStateRef.update { gameState =>
      val newPlayer      = Player(player, PlayerColors(gameState.players.length))
      val updatedPlayers = newPlayer :: gameState.players

      webSocketHub.broadcast(colorSystemMessage(s"$player joined the game"))
      gameState.copy(players = updatedPlayers)
    }
  }


  // -----manage player turns -------------------------------//
  private def setRandomStartingPlayer(): IO[Unit] =
    gameStateRef.update { gameState =>
      val index = Random.nextInt(gameState.players.length)

      webSocketHub.broadcast(colorPlayerMessage(gameState.players(index), "'s starting"))
      gameState.copy(currentPlayerIndex = index)
    }

  private def nextPlayer(): IO[Unit] =
    gameStateRef.update { gameState =>
      val index = gameState.currentPlayerIndex + 1 match {
        case x if (1 until gameState.players.length).contains(x) => x
        case _                                                   => 0
      }

      gameState.copy(currentPlayerIndex = index)
    }

  private def previousPlayer(): IO[Unit] =
    gameStateRef.update { gameState =>
      val index = gameState.currentPlayerIndex - 1 match {
        case x if x < 0 => gameState.players.length - 1
        case x          => x
      }

      gameState.copy(currentPlayerIndex = index)
    }

  private def setNextPlayer(player: Player): IO[Unit] =
    gameStateRef.update { gameState =>
      val index = gameState.players.indexOf(player)

      gameState.copy(currentPlayerIndex = index)
    }

  // ^^^^manage player turns^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^//

  // ----manage win and lose----------------------------------//
  private def killCurrentPlayer: IO[Unit] =
    gameStateRef.update { gameState =>
      val currentIndex  = gameState.currentPlayerIndex
      val (left, right) = gameState.players.splitAt(currentIndex)
      val newPlayers    = left ::: right.drop(1)

      webSocketHub.broadcast(diedMessage(right.head)) // > avoid head
      gameState.copy(players = newPlayers)

    }

  private def getWinner: IO[Option[Player]] =
    gameStateRef.get.map { gameState =>
      gameState.players.length match {
        case 1 => Some(gameState.players.head)
        case _ => None
      }
    }

  // ___________ Deck Operations ______________________________//

  private def addCardToDiscardDeck(card: Card): IO[Unit] =
    gameStateRef.update { gameState =>
      val newDiscardDeck = gameState.discardDeck.prepend(card)
      gameState.copy(discardDeck = newDiscardDeck)
    }

  private def drawCard(): IO[Card] = {
    gameStateRef.modify { gameState =>
      val drawDeck = gameState.drawDeck

      if (drawDeck.length == 0)
        switchPiles()

      val (deck, card) = gameState.drawDeck.draw

      (gameState.copy(drawDeck = deck), card)

    }
  }

  private def switchPiles(): IO[Unit] = {
    gameStateRef.update { gameState =>
      val draw = Deck.initShuffledFromDiscardPile(gameState.discardDeck)

      webSocketHub.broadcast(colorSystemMessage(s"switching piles"))
      gameState.copy(drawDeck = draw, discardDeck = Deck(List.empty))
    }

  }

  private def switchPiles2(): IO[Unit] = {
    gameStateRef.update { gameState =>
      val draw = Deck.initShuffledFromDiscardPile2(gameState.drawDeck, gameState.discardDeck)

      webSocketHub.broadcast(colorSystemMessage(s"switching piles"))
      gameState.copy(drawDeck = draw, discardDeck = Deck(List.empty))
    }

  }

  private def handCards(): IO[Unit] = {
    webSocketHub.broadcast(colorSystemMessage(s"\nHanding cards...\n")) *> {
      gameStateRef.update { gameState =>
        val (deckWOBombs, bombs) = removeDefuseAndBombs(gameState.drawDeck)
        val (deck, map) = gameState.players.foldLeft((List.empty[Card], Map.empty[PlayerID, Hand])) {
          case ((cards, map), p) =>
            val (hand, newDrawDeck) = deckWOBombs.splitAt(7)
            val newHand             = hand :+ Defuse

            (cards ++ newDrawDeck, map + (p.playerID -> newHand))
        }

        val newDrawDeck = Deck(deck ++ bombs).shuffled

        gameState.copy(drawDeck = newDrawDeck, playersHands = map)

      }
    }
  }

  private def getFirst3Drawn: IO[List[Card]] = {
    gameStateRef.get.map { gameState =>
      val drawDeck = gameState.drawDeck

      if (drawDeck.length - 3 <= 0)
        switchPiles2()

      gameState.drawDeck.getFirstN(3)
    }
  }

  private def updateDrawDeck(function: Deck => Deck): IO[Unit] =
    gameStateRef.update { gameState =>
      gameState.copy(drawDeck = function(gameState.drawDeck))
    }

  private def updateDiscardDeck(function: Deck => Deck): IO[Unit] =
    gameStateRef.update { gameState =>
      gameState.copy(discardDeck = function(gameState.discardDeck))
    }

  /// players hand
  /** Tries to find a defuse in the cards the player is holding
    * @return
    *   Option of defuse card
    */
  private def tryGetDefuse(playerID: PlayerID): IO[Option[Int]] =
    gameStateRef.get.map { gameState =>
      val hand = gameState.playersHands.getOrElse(playerID, List.empty)
      hand.zipWithIndex
        .collectFirst { case (Defuse, i) => i
        }
    }


  /** Plays the card at given index, removing it from the player's hand and returning the card
    * @param index
    *   \- the index of the card to play
    * @return
    *   Card played
    */
  private def playCard(playerID: PlayerID, index: Int): IO[Card] =
    gameStateRef.modify { gameState =>
      val hands         = gameState.playersHands
      val card          = hands(playerID)(index)
      val (left, right) = hands(playerID).splitAt(index)
      val newCards      = left ::: right.drop(1)

      (
        gameState.copy(
          discardDeck = gameState.discardDeck.prepend(card),
          playersHands = hands + (playerID -> newCards)
        ),
        card
      )
    }

  /** Adds a card to the players' hand
    * @param card
    *   \- card to add
    */
  private def drawCard(playerID: PlayerID, card: Card): Unit =
    gameStateRef.update { gameState =>
      val hands    = gameState.playersHands
      val newCards = hands(playerID) :+ card
      gameState.copy(playersHands = hands + (playerID -> newCards))
    }

  /** returns a string representation of the players' hand, 1-indexed
    * @return
    *   IO of string representation of the hand
    */
  private def getHandWithIndex(playerID: PlayerID): IO[String] =
    gameStateRef.get.map { gameState =>
      gameState.playersHands
        .getOrElse(playerID, List.empty[Card])
        .zipWithIndex
        .map { // > print error?
          case (card, i) => s"${i + 1}. $card   "
        }
        .mkString
    }

  // ----inputs----//
  private def alterTheFuture(playerID: PlayerID): IO[String] = {

    for {
      cards3 <- getFirst3Drawn
      _      <- webSocketHub.sendToPlayer(playerID, "Next three cards are: \n")
      _ <- webSocketHub.sendToPlayer(
        playerID,
        s"${cards3.zipWithIndex.foldLeft("") { case (acc, (card, i)) =>
            acc ++ s"${i + 1}. $card\n"
          }}"
      )
      _      <- webSocketHub.sendToPlayer(playerID, "Insert new card order (e.g. 213) >>")
      string <- webSocketHub.getGameInput.map(_.replaceAll(" ", ""))
      valid <- string.toSet match {
        case set if set == Set('1', '2', '3') => string.pure[IO]
        case _ =>
          webSocketHub.sendToPlayer(
            playerID,
            colorErrorMessage("Invalid input, please specify order using only numbers")
          ) *> alterTheFuture(
            playerID
          )
      }
    } yield valid
  }

  private def buryCard(player: Player, card: Card): IO[Unit] = {
    for {
      deckLength <- gameStateRef.get.map(_.drawDeck.length)
      _          <- webSocketHub.sendToPlayer(player.playerID, "Where do you want to bury this card?")
      _          <- webSocketHub.sendToPlayer(player.playerID, s"Insert a number between 1 and $deckLength >> ")
      string     <- webSocketHub.getGameInput.map(_.trim.toIntOption)
      _ <- string match {
        case Some(index) if (0 until deckLength).contains(index - 1) =>
          updateDrawDeck(_.insertAt(index - 1, card))
        case _ =>
          webSocketHub.sendToPlayer(player.playerID, colorErrorMessage("Invalid input")) *> buryCard(player, card)
      }

    } yield ()

  }

  private def targetAttack(player: Player): IO[Player] =
    for {
      players <- gameStateRef.get.map(_.players)
      _       <- webSocketHub.sendToPlayer(player.playerID, "\nWho do you want to target? \n")
      _ <- webSocketHub.sendToPlayer(
        player.playerID,
        s"${players.filterNot(_.playerID == player.playerID).zipWithIndex.foldLeft("") { case (acc, (p, i)) =>
            acc ++ s"${i + 1}. ${p.playerID}\n"
          }}"
      )
      _      <- webSocketHub.sendToPlayer(player.playerID, "Insert index >> ")
      string <- webSocketHub.getGameInput.map(_.toIntOption)
      valid <- string match {
        case Some(value) =>
          value match {
            case x if (1 until players.length) contains x =>
              players.filterNot(_.playerID == player.playerID)(x - 1).pure[IO]
            case _ =>
              webSocketHub.sendToPlayer(player.playerID, colorErrorMessage("Invalid index")) *> targetAttack(player)
          }
        case None =>
          webSocketHub.sendToPlayer(player.playerID, colorErrorMessage("Invalid input")) *> targetAttack(player)
      }
    } yield valid

  // game loops
  def initialize(): IO[Unit] = {
    for {
      _              <- webSocketHub.broadcast(gameTitleBanner)
      _              <- webSocketHub.broadcast(colorSystemMessage(s"\r\nInitializing..."))
      _              <- handCards()
      _              <- setRandomStartingPlayer()
      _                <- setRandomStartingPlayer()
      _              <- gameLoop()

    } yield ()
  }

  private def gameLoop(): IO[Unit] = {
    for {
      _     <- gameStateRef.get.map(gameState => gameState.players(gameState.currentPlayerIndex))
      _          <- playerTurn
      nextPlayer <- nextPlayer()
    } yield nextPlayer

    getWinner.flatMap({
      case Some(player) => webSocketHub.broadcast(colorPlayerMessage(player, " won the game"))
      case None         => nextPlayer() *> gameLoop()
    })

  }

  private def playerTurn: IO[Unit] = {
    for {
      _         <- webSocketHub.broadcast(s"\n$TurnSeparator\n")
      gameState <- gameStateRef.get
      player = gameState.players(gameState.currentPlayerIndex)
      _ <- IO.println(s"discard: ${gameState.discardDeck}") //
      _ <- IO.println(s"draw: ${gameState.drawDeck}")       //
      _ <- webSocketHub.broadcast(colorPlayerMessage(player, "'s turn"))
      _ <- webSocketHub.sendToPlayer(player.playerID, s"\nYour hand is: \n ${getHandWithIndex(player.playerID)}")

      playOrPass <- askPlayOrPass(player) // Does player want to play a card?
      cardOpt <- playOrPass.fold(Option.empty[Card].pure[IO])(_ => askForCard(player.playerID)) // Which card?
      playerSkipped <- cardOpt.fold(false.pure[IO])(card => {
        updateDiscardDeck(_.prepend(card))
        handleCardPlayed(player, card)
      }) // If card played, does player skip draw?
      _ <-
        if (!playerSkipped) {
          for {
            card <- drawCard()
            _    <- webSocketHub.sendToPlayer(player.playerID, s"\n$card drawn")

            _ <- card match {
              case ExplodingKitten =>
                for {
                  _ <- webSocketHub.broadcast(s"\n${player.playerID} drew a $card")
                  _ <- addCardToDiscardDeck(ExplodingKitten)
                  defuseOpt <- tryGetDefuse(player.playerID)
                  _ <- defuseOpt.fold(killCurrentPlayer)(index =>
                    playCard(player.playerID, index) *> webSocketHub.broadcast(s"$Defuse used")
                  )
                } yield ()
              case card => drawCard(player.playerID, card).pure[IO]
            }
          } yield ()
        } else IO.unit

    } yield ()
  }

  private def askPlayOrPass(player: Player): IO[Option[Boolean]] =
    for {
      _      <- webSocketHub.sendToPlayer(player.playerID, s"\n${player.playerID}, do you wish to play a card? (y/n)")
      answer <- IO.readLine.map(_.trim.toLowerCase)
      // answer <- player.receiveQueue.take.map(_.trim.toLowerCase)
      result <- answer match {
        case "y" => Some(true).pure[IO]
        case "n" => None.pure[IO]
        case _ =>
          webSocketHub.sendToPlayer(player.playerID, colorErrorMessage("Invalid input")) *> askPlayOrPass(player)
      }
    } yield result

  private def askForCard(playerID: PlayerID): IO[Option[Card]] =
    for {
      _          <- webSocketHub.sendToPlayer(playerID, "\nEnter the index of the card you want to play (c to cancel).")
      playerHand <- gameStateRef.get.map(_.playersHands.getOrElse(playerID, List.empty)) // > error?
      cardOpt <- webSocketHub.getGameInput.map(_.trim.toLowerCase).flatMap {
        case "c" => None.pure[IO]
        case x if x.toIntOption.isDefined =>
          x.toInt - 1 match {
            case i if playerHand.indices contains i =>
              playerHand(i) match {
                case ExplodingKitten | Defuse | Nope =>
                  webSocketHub
                    .sendToPlayer(playerID, colorErrorMessage("You can't play this card right now")) *> askForCard(
                    playerID
                  )
                case _ => playCard(playerID, i).map(Some(_))
              }

            case _ => webSocketHub.sendToPlayer(playerID, colorErrorMessage("Invalid index")) *> askForCard(playerID)
          }
        case _ => webSocketHub.sendToPlayer(playerID, colorErrorMessage("Invalid input")) *> askForCard(playerID)
      }
    } yield cardOpt

  private def handleCardPlayed(player: Player, card: Card): IO[Boolean] = {
    card match {
      case Shuffle =>
        updateDrawDeck(_.shuffled) *> false.pure[IO]

      case Skip => true.pure[IO]

      case AlterTheFuture3X =>
        for {
          order <- alterTheFuture(player.playerID)
          _     <- updateDrawDeck(_.alterTheFuture3X(order))
        } yield false

      case SwapTopAndBottom =>
        updateDrawDeck(_.swapTopAndBottom) *> false.pure[IO]
        false.pure[IO]

      case Attack2X =>
        for {
          _ <- nextPlayer()
          _ <- playerTurn
          _ <- previousPlayer()
        } yield true

      case TargetedAttack2X =>
        for {
          nextPlayer <- targetAttack(player)
          _          <- setNextPlayer(nextPlayer)
          _          <- playerTurn
          _          <- previousPlayer()
        } yield true

      case CatomicBomb =>
        updateDrawDeck(_.withExplodingKittensOnTop) *> true.pure[IO]

      case Bury =>
        for {
          card <- drawCard()
          _    <- buryCard(player, card)
        } yield true

      case Reverse =>
        updateDrawDeck(_.reversed) *> false.pure[IO]

      case Tacocat | FeralCat => false.pure[IO]

    }
  }

}
object Game {
  def create(
      nPlayers: Int,
      webSocketHub: WebSocketHub
  ): IO[Game] = {
    val initialDrawDeck    = initShuffledNopeSauce(nPlayers)
    val initialDiscardDeck = Deck(List.empty)
    val currentPlayerIndex = -1
    val playersList        = List.empty

    val initialState = State(initialDrawDeck, initialDiscardDeck, currentPlayerIndex, playersList, Map.empty)

    for {
      // manager <- StateManager.of(initialState)
      gameStateRef <- Ref.of[IO, State](initialState)
    } yield new Game(nPlayers, webSocketHub, gameStateRef)
  }
}

/*private def askForNope(): IO[Boolean] = { // Nope only functional when multiplayer
  /*    for {
        player <- players
        hand = player.hand
        if hand.contains(Nope())
        _ <- printlnForPlayer(player, s"${player.playerID}, do you wish to nope this action? (y/n)")
        res <- askPlayOrPass(player)
        bool <- res.fold(false.pure[IO])(answer => answer.pure[IO])
      } yield bool*/

  true.pure[IO]
}*/
