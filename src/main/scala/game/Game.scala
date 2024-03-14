package game

import card._
import card.Deck._
import cats.effect.{IO, Ref}
import cats.implicits._
import gamestate.Command._
import gamestate._
import players.Player
import players.Player.{Hand, PlayerID}
import utils.TerminalUtils._
import websockethub.WebSocketHub

import scala.util.Random

case class Game(
    nPlayers: Int,
    webSocketHub: WebSocketHub,
    gameStateRef: Ref[IO, State],
    stateManager: StateManager
) {
  // Cmd  ✔
  def joinGame(player: PlayerID): IO[Unit] =
    gameStateRef.update { gameState =>
      val newPlayer      = Player(player, PlayerColors(gameState.players.length))
      val updatedPlayers = newPlayer :: gameState.players
      gameState.copy(players = updatedPlayers)
    } *> webSocketHub.broadcast(colorSystemMessage(s"$player joined the game")) *> stateManager.tell(AddPlayer(player))


  def playerDisconnected(playerID: PlayerID): IO[Unit] =
    IO.println(s"DISC: $playerID")

  // -----manage player turns -------------------------------//
  private def setRandomStartingPlayer(): IO[Unit] =
    gameStateRef
      .modify { gameState =>
        val index = Random.nextInt(gameState.players.length)

        (gameState.copy(currentPlayerIndex = index), gameState.players(index))
      }
      .flatMap(player => webSocketHub.broadcast(colorPlayerMessage(player, "'s starting\n"))) *> stateManager.tell(
      SetRandomPlayerTurn()
    )

  private def nextPlayer(): IO[Unit] =
    gameStateRef.update { gameState =>
      val index = gameState.currentPlayerIndex + 1 match {
        case x if (1 until gameState.players.length).contains(x) => x
        case _                                                   => 0
      }
      println(gameState.currentPlayerIndex)
      println(index)
      gameState.copy(currentPlayerIndex = index)
    } // *> stateManager.tell(NextPlayerTurn())

  private def previousPlayer(): IO[Unit] =
    gameStateRef.update { gameState =>
      val index = gameState.currentPlayerIndex - 1 match {
        case x if x < 0 => gameState.players.length - 1
        case x          => x
      }

      gameState.copy(currentPlayerIndex = index)
    } // *> stateManager.tell(PreviousPlayerTurn())

  private def setNextPlayer(player: Player): IO[Unit] =
    gameStateRef.update { gameState =>
      val index = gameState.players.indexOf(player)
      gameState.copy(currentPlayerIndex = index)
    } // *> stateManager.tell(SetPlayerTurn(player.playerID))

  // ^^^^manage player turns^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^//

  // ----manage win and lose----------------------------------//

  // Cmd ✔
  private def killCurrentPlayer: IO[Unit] =
    gameStateRef
      .modify { gameState =>
        val currentIndex  = gameState.currentPlayerIndex
        val (left, right) = gameState.players.splitAt(currentIndex)
        val newPlayers    = left ::: right.drop(1)

        val index = currentIndex - 1 match {
          case x if x < 0 => gameState.players.length - 1
          case x          => x
        }

        (gameState.copy(players = newPlayers, currentPlayerIndex = index), right.head) // > avoid head

      }
      .flatMap(player => webSocketHub.broadcast(diedMessage(player))) // *> stateManager.tell(KillCurrentPlayer())

  private def getWinner: IO[Option[Player]] =
    gameStateRef.get.map { gameState =>
      gameState.players.length match {
        case 1 => Some(gameState.players.head)
        case _ => None
      }
    }

  // ___________ Deck Operations ______________________________//

  private def drawCard(): IO[Card] =
    for {
      cards <- gameStateRef.get.map(_.drawDeck.length == 0)
      _     <- if (cards) switchPiles2() else IO.unit

      res <- gameStateRef.modify { gameState =>
        val (deck, card)  = gameState.drawDeck.draw
        val currentPlayer = gameState.players(gameState.currentPlayerIndex).playerID
        val playerHand    = gameState.playersHands(currentPlayer) :+ card

        card match {
          case ExplodingKitten =>
            (gameState.copy(drawDeck = deck, discardDeck = gameState.discardDeck.prepend(ExplodingKitten)), card)
          case _ =>
            (
              gameState.copy(drawDeck = deck, playersHands = gameState.playersHands + (currentPlayer -> playerHand)),
              card
            )

        }
      }
      card = res
    } yield card

  private def switchPiles2(): IO[Unit] =
    webSocketHub.broadcast(colorSystemMessage(s"Switching piles...\n")) *>
      gameStateRef.update { gameState =>
        val draw = Deck.initShuffledFromDiscardPile2(gameState.drawDeck, gameState.discardDeck)
        gameState.copy(drawDeck = draw, discardDeck = Deck(List.empty))
      }

  private def handCards(): IO[Unit] = {
    webSocketHub.broadcast(colorSystemMessage(s"Handing cards...\n")) *> {
      gameStateRef.update { gameState =>
        val (deckWOBombs, bombs) = removeDefuseAndBombs(gameState.drawDeck)
        val (deck, map) = gameState.players.foldLeft((List.empty[Card], Map.empty[PlayerID, Hand])) {
          case ((cards, map), p) =>
            val (hand, newDrawDeck) = deckWOBombs.splitAt(7)
            val newHand             = hand :+ Defuse

            (cards ++ newDrawDeck, map + (p.playerID -> newHand))
        }

        val newDrawDeck = Deck(deck ++ bombs).shuffled
        println(newDrawDeck)
        gameState.copy(drawDeck = newDrawDeck, playersHands = map)

      }
    }
  }
  private def updateDrawDeck(function: Deck => Deck): IO[Unit] =
    gameStateRef.update { gameState =>
      gameState.copy(drawDeck = function(gameState.drawDeck))
    }

  /// players hand

  /** Tries to find a defuse in the cards the player is holding
    *
    * @return
    *   Option of defuse card
    */
  private def tryGetDefuse(playerID: PlayerID): IO[Option[Int]] =
    gameStateRef.get.map { gameState =>
      val hand = gameState.playersHands.getOrElse(playerID, List.empty)
      hand.zipWithIndex
        .collectFirst { case (Defuse, i) => i }
    }

  /** Plays the card at given index, removing it from the player's hand and returning the card
    *
    * @param index
    *   \- the index of the card to play
    * @return
    *   Card played
    */
  private def playCard(index: Int): IO[Card] =
    gameStateRef.modify { gameState =>
      val currentPlayer = gameState.players(gameState.currentPlayerIndex)
      val hands         = gameState.playersHands
      val card          = hands(currentPlayer.playerID)(index)
      val (left, right) = hands(currentPlayer.playerID).splitAt(index)
      val newCards      = left ::: right.drop(1)

      (
        gameState.copy(
          discardDeck = gameState.discardDeck.prepend(card),
          playersHands = hands + (currentPlayer.playerID -> newCards)
        ),
        card
      )
    }

  /** returns a string representation of the players' hand, 1-indexed
    *
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
  private def alterTheFuture(cards3: List[Card], playerID: PlayerID): IO[String] = {
    for {
      _ <- webSocketHub.sendToPlayer(playerID, "Next three cards are: \n")
      _ <- webSocketHub.sendToPlayer(
        playerID,
        s"${cards3.zipWithIndex.foldLeft("") { case (acc, (card, i)) =>
            acc ++ s"${i + 1}. $card\n"
          }}"
      )
      _      <- webSocketHub.sendToPlayer(playerID, "Insert new card order (e.g. 213) >>")
      string <- webSocketHub.getGameInput
      valid <- string.toSet match {
        case set if set == Set('1', '2', '3') => string.pure[IO]
        case s =>
          webSocketHub.sendToPlayer(
            playerID,
            colorErrorMessage(s"Invalid input $s, please specify order using only numbers")
          ) *> alterTheFuture(cards3, playerID)
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
      _ <- webSocketHub.broadcast(gameTitleBanner)
      _ <- webSocketHub.broadcast(colorSystemMessage(s"Initializing..."))
      _ <- handCards()
      _ <- setRandomStartingPlayer()
      _ <- gameLoop()
    } yield ()
  }

  private def gameLoop(): IO[Unit] = {
    val loop = for {
      _          <- playerTurn()
      nextPlayer <- nextPlayer()
    } yield nextPlayer

    loop *> getWinner.flatMap({
      case Some(player) => webSocketHub.broadcast(colorPlayerMessage(player, " won the game"))
      case None         => gameLoop()
    })
  }

  private def playerTurn(): IO[Unit] = {
    for {
      _ <- webSocketHub.broadcast(s"\n$TurnSeparator\n")

      gameState <- gameStateRef.get
      player = gameState.players(gameState.currentPlayerIndex)

      _ <- IO.println(s"discard: ${gameState.discardDeck}") //
      _ <- IO.println(s"draw: ${gameState.drawDeck}")       //

      _    <- webSocketHub.broadcast(colorPlayerMessage(player, "'s turn"))
      hand <- getHandWithIndex(player.playerID)
      _    <- webSocketHub.sendToPlayer(player.playerID, s"Your hand is: $hand")

      playOrPass <- playOrPassPrompt(player, gameState) // Does player want to play a card?
      playerSkipped <- playOrPass.fold(false.pure[IO])(i => {
        handleCardPlayed(player, playCard(i))
      }) // If card played, does player skip draw?
      _ <-
        if (!playerSkipped) {
          for {
            card <- drawCard()
            _    <- webSocketHub.sendToPlayer(player.playerID, s"$card drawn")
            _ <- card match {
              case ExplodingKitten =>
                for {
                  _         <- webSocketHub.broadcast(s"${player.playerID} drew $card")
                  defuseOpt <- tryGetDefuse(player.playerID)
                  _ <- defuseOpt.fold(killCurrentPlayer)(index =>
                    playCard(index) *> webSocketHub.broadcast(s"$Defuse used")
                  )
                } yield ()
              case _ => IO.unit
            }
          } yield ()
        } else IO.unit

    } yield ()
  }

  private def playOrPassPrompt(player: Player, state: State): IO[Option[Int]] =
    for {
      _ <- webSocketHub.sendToPlayer(player.playerID, colorPlayerMessage(player, s", do you wish to play a card?"))
      playerHand = state.playersHands(player.playerID)

      _ <- webSocketHub.sendToPlayer(player.playerID, "Enter the index of the card you want to play (n to Pass) >> ")
      answer <- webSocketHub.getGameInput.map(_.trim.toLowerCase)

      result <- answer match {
        case "n" => None.pure[IO]
        case x if x.toIntOption.isDefined =>
          x.toInt - 1 match {
            case i if playerHand.indices contains i =>
              playerHand(i) match {
                case ExplodingKitten | Defuse | Nope =>
                  webSocketHub.sendToPlayer(
                    player.playerID,
                    colorErrorMessage("You can't play this card right now")
                  ) *> playOrPassPrompt(player, state)
                case _ => Some(i).pure[IO]
              }

            case _ =>
              webSocketHub.sendToPlayer(player.playerID, colorErrorMessage("Invalid index")) *> playOrPassPrompt(
                player,
                state
              )
          }

        case _ =>
          webSocketHub.sendToPlayer(player.playerID, colorErrorMessage("Invalid input")) *> playOrPassPrompt(
            player,
            state
          )
      }
    } yield result

  private def handleCardPlayed(player: Player, ioCard: IO[Card]): IO[Boolean] =
    for {
      card <- ioCard
      _    <- webSocketHub.broadcast(colorPlayerMessage(player, s" played $card"))
      skipped <- card match {
        case Shuffle =>
          updateDrawDeck(_.shuffled) *> false.pure[IO]

        case Skip => true.pure[IO]

        case AlterTheFuture3X =>
          for {
            enoughCards <- gameStateRef.get.map(_.drawDeck.length < 3)
            _           <- if (enoughCards) switchPiles2() else IO.unit
            cards3      <- gameStateRef.get.map(_.drawDeck.getFirstN(3))
            order       <- alterTheFuture(cards3, player.playerID)
            _           <- updateDrawDeck(_.alterTheFuture3X(order))
          } yield false

        case SwapTopAndBottom =>
          updateDrawDeck(_.swapTopAndBottom) *> false.pure[IO]

        case Attack2X =>
          for {
            _ <- nextPlayer()
            _ <- playerTurn()
            _ <- previousPlayer()
          } yield true

        case TargetedAttack2X =>
          for {
            nextPlayer <- targetAttack(player)
            _          <- setNextPlayer(nextPlayer)
            _          <- playerTurn()
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
    } yield skipped

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
      stateManager <- StateManager.of(initialState, webSocketHub)
      gameStateRef <- Ref.of[IO, State](initialState)
    } yield new Game(nPlayers, webSocketHub, gameStateRef, stateManager)
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
