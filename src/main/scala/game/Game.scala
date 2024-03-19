package game

import card._
import card.Deck._
import cats.effect.{Deferred, IO, Ref}
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
    gameStateRef: Ref[IO, State],
    stateManager: StateManager
) {

  /** Creates a player with PlayerID and joins the game
   * @param playerID
   *   id of the player
   */
  def joinGame(playerID: PlayerID): IO[Unit] =
    (for {
      deferred <- Deferred[IO, Boolean]
      _ <- gameStateRef.update { gameState =>
        val disconnectedPlayers = gameState.disconnections + (playerID -> deferred)
        val newPlayer           = Player(playerID, PlayerColors(gameState.players.length))
        val updatedPlayers      = newPlayer :: gameState.players
        gameState.disconnections + (playerID -> Deferred)
        gameState.copy(players = updatedPlayers, disconnections = disconnectedPlayers)
      }
    } yield ()) *> webSocketHub.broadcast(colorSystemMessage(s"$playerID joined the game"))

  /** Callback that warns the game the player disconnected
   * @param playerID
   *   player disconnected
   */
  def playerDisconnected(playerID: PlayerID): IO[Unit] =
    IO.println(s"$playerID disconnected from game") *> previousPlayer() *> {
      for {
        gameState <- gameStateRef.get
        deferred  <- IO.fromOption(gameState.disconnections.get(playerID))(new RuntimeException("Deferred not found"))
        _         <- deferred.complete(true)
      } yield ()
    } *> gameStateRef.update(gameState => {
      val newPlayers = gameState.players.filterNot(_.playerID == playerID)

      gameState.copy(players = newPlayers)
    }) *> webSocketHub.broadcast(colorSystemMessage(s"$playerID left"))

  // -----manage player turns -------------------------------//

  /** Sets the current player index to a random number, bounded by how many players are in-game
   */
  private def setRandomPlayerNext(): IO[Unit] =
    gameStateRef
      .modify { gameState =>
        val index = Random.nextInt(gameState.players.length)

        (gameState.copy(currentPlayerIndex = index), gameState.players(index))
      }
      .flatMap(player => webSocketHub.broadcast(colorPlayerMessage(player, "'s starting\n")))


  /** Sets the current player index to the next player
   */
  private def nextPlayer(): IO[Unit] =
    gameStateRef.update { gameState =>
      val index = gameState.currentPlayerIndex + 1 match {
        case x if (1 until gameState.players.length).contains(x) => x
        case _                                                   => 0
      }
      println(gameState.currentPlayerIndex)
      println(index)
      gameState.copy(currentPlayerIndex = index)
    }

  /** Sets the current player index to the previous player
   */
  private def previousPlayer(): IO[Unit] =
    gameStateRef.update { gameState =>
      val index = gameState.currentPlayerIndex - 1 match {
        case x if x < 0 => gameState.players.length - 1
        case x          => x
      }

      gameState.copy(currentPlayerIndex = index)
    }


  /** Sets the current player index to the index of a given player
   * @param player
   *   player to set as the next one
   */
  private def setNextPlayer(player: Player): IO[Unit] =
    gameStateRef.update { gameState =>
      val index = gameState.players.indexOf(player)
      gameState.copy(currentPlayerIndex = index)
    }

  // ^^^^manage player turns^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^//

  // ----manage win and lose----------------------------------//

  /** Kills the current player, removing them from the game
   */
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

  /** Checks if the game currently has a winner, meaning there is only one player left
   * @return
   *   None if there's no winner or Some(player) that won the game
   */
  private def getWinner: IO[Option[Player]] =
    gameStateRef.get.map { gameState =>
      gameState.players.length match {
        case 1 => Some(gameState.players.head)
        case _ => None
      }
    }

  // ___________ Deck Operations ______________________________//


  /**
   * Draws a card from the draw deck, if no cards are available, piles are switched and shuffled
   * If the card is a exploding kitten it's discarded, otherwise it's added to the current player's hand
   * @return the card drawn
   */
  private def drawCard(): IO[Card] =
    for {
      cards <- gameStateRef.get.map(_.drawDeck.length == 0)
      _     <- if (cards) switchPiles2() else IO.unit

      card <- gameStateRef.modify { gameState =>
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
    } yield card

  /**
   * Adds the remaining cards from the draw deck and the discard deck and shuffles them into a new draw deck.
   * Clears the discard deck
   */
  private def switchPiles2(): IO[Unit] =
    webSocketHub.broadcast(colorSystemMessage(s"Switching piles...\n")) *>
      gameStateRef.update { gameState =>
        val draw = Deck.initShuffledFromDiscardPile2(gameState.drawDeck, gameState.discardDeck)
        gameState.copy(drawDeck = draw, discardDeck = Deck(List.empty))
      }


  /**
   * Removes all bombs and defuses from the deck and deals the players 7 cards + a defuse then adds the bombs back and shuffles the deck
   */
  private def handCards(): IO[Unit] = {
    webSocketHub.broadcast(colorSystemMessage(s"Handing cards...\n")) *> {
      gameStateRef.update { gameState =>
        val (deckWOBombs, bombs) = removeDefuseAndBombs(gameState.drawDeck)
        val (deck, map) = gameState.players.foldLeft((deckWOBombs, Map.empty[PlayerID, Hand])) {
          case ((cards, map), p) =>
            val (hand, newDrawDeck) = cards.splitAt(7)
            val newHand             = hand :+ Defuse

            (newDrawDeck, map + (p.playerID -> newHand))
        }

        val newDrawDeck = Deck(deck ++ bombs).shuffled
        gameState.copy(drawDeck = newDrawDeck, playersHands = map)

      }
    }
  }

  /**
   * Updates the draw deck according to a given function
   * @param function function to apply on the deck
   */
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
      string <- webSocketHub.getGameInput(playerID)
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

  private def buryCard(playerID: PlayerID, card: Card): IO[Unit] = {
    for {
      deckLength <- gameStateRef.get.map(_.drawDeck.length)
      _          <- webSocketHub.sendToPlayer(playerID, "Where do you want to bury this card?")
      _          <- webSocketHub.sendToPlayer(playerID, s"Insert a number between 1 and $deckLength >> ")
      string     <- webSocketHub.getGameInput(playerID).map(_.toIntOption)
      _ <- string match {
        case Some(index) if (0 until deckLength).contains(index - 1) =>
          updateDrawDeck(_.insertAt(index - 1, card))
        case _ =>
          webSocketHub.sendToPlayer(playerID, colorErrorMessage("Invalid input")) *> buryCard(playerID, card)
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
            acc ++ s"${i + 1}. ${p.playerID}       "
          }}"
      )
      _      <- webSocketHub.sendToPlayer(player.playerID, "Insert index >> ")
      string <- webSocketHub.getGameInput(player.playerID).map(_.toIntOption)
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
      _ <- setRandomPlayerNext()
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

  private def playerTurn(): IO[Unit] = IO
    .race( // On current player disconnected - break
      for {
        gameState <- gameStateRef.get
        playerID = gameState.players(gameState.currentPlayerIndex).playerID
        deferred <- IO.fromOption(gameState.disconnections.get(playerID))(new RuntimeException("Deferred not found"))
        _        <- deferred.get
      } yield (),
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
    )
    .void

  private def playOrPassPrompt(player: Player, state: State): IO[Option[Int]] =
    for {
      _ <- webSocketHub.sendToPlayer(player.playerID, colorPlayerMessage(player, s", do you wish to play a card?"))
      playerHand = state.playersHands(player.playerID)

      _ <- webSocketHub.sendToPlayer(
        player.playerID,
        "Enter the index of the card you want to play (n to Pass or index -h to print card description) (to use cat cards combo: e.g. 1,2 or 1,2,3)>> "
      )
      answer <- webSocketHub.getGameInput(player.playerID)

      result <- answer match {
        case "n" => None.pure[IO]
        case s"$index -h" if index.toIntOption.isDefined =>
          index.toInt - 1 match {
            case i if playerHand.indices contains i =>
              webSocketHub.sendToPlayer(
                player.playerID,
                colorErrorMessage(playerHand(i).toStringDescription)
              ) *> playOrPassPrompt(
                player,
                state
              )
            case _ =>
              webSocketHub.sendToPlayer(
                player.playerID,
                colorErrorMessage("Invalid index (e.g.: 1 -h)")
              ) *> playOrPassPrompt(
                player,
                state
              )
          }
        case s"${c1},${c2}" if c1.toIntOption.isDefined && c2.toIntOption.isDefined =>
          {
            (c1.toInt, c2.toInt).toList match {
              case list if list.forall(i => {
                    playerHand.indices.contains(i - 1) && (playerHand(i - 1) match {
                      case Tacocat | FeralCat => true
                      case _                  => false
                    })
                  }) =>
                list.traverse(i => playCard(i-1)) *> stealFromPlayer(
                  player.playerID,
                  state.players,
                  isRandom = true
                )
              case _ =>
                webSocketHub.sendToPlayer(
                  player.playerID,
                  "Invalid input, play 2 indices of cat cards (e.g.: 1,2)"
                ) *> playOrPassPrompt(player, state)
            }
          } *> None.pure[IO]
        case s"${c1},${c2},${c3}" if c1.toIntOption.isDefined && c2.toIntOption.isDefined && c3.toIntOption.isDefined =>
          {
            (c1.toInt, c2.toInt, c3.toInt).toList match {
              case list if list.forall(i => {
                    playerHand.indices.contains(i - 1) && (playerHand(i - 1) match {
                      case Tacocat | FeralCat => true
                      case _                  => false
                    })
                  }) => list.traverse(i => playCard(i-1)) *> stealFromPlayer(
                  player.playerID,
                  state.players,
                  isRandom = false
                )
              case _ =>
                webSocketHub.sendToPlayer(
                  player.playerID,
                  "Invalid input, play 3 indices of cat cards (e.g.: 1 2 3"
                ) *> playOrPassPrompt(player, state)
            }
          } *> None.pure[IO]
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

  private def stealFromPlayer(playerID: PlayerID, players: List[Player], isRandom: Boolean): IO[Unit] =
    for {
      _ <- webSocketHub.sendToPlayer(playerID, "Who do you want to steal from?")
      _ <- webSocketHub.sendToPlayer(
        playerID,
        s"${players.filterNot(_.playerID == playerID).zipWithIndex.foldLeft("") { case (acc, (p, i)) =>
            acc ++ s"${i + 1}. ${p.playerID}       "
          }}"
      )
      _      <- webSocketHub.sendToPlayer(playerID, "Insert index >> ")
      string <- webSocketHub.getGameInput(playerID).map(_.toIntOption)
      _ <- string match {
        case Some(value) =>
          value match {
            case x if (1 until players.length) contains x =>
              if (isRandom)
                stealCard(fromID = players.filterNot(_.playerID == playerID)(x - 1).playerID, toID = playerID, None)
              else
                pickFromPlayer(
                  fromID = players.filterNot(_.playerID == playerID)(x - 1).playerID,
                  toID = playerID
                ).flatMap(card => stealCard(fromID = players.filterNot(_.playerID == playerID)(x - 1).playerID, toID = playerID, Some(card)))
            case _ =>
              webSocketHub.sendToPlayer(playerID, colorErrorMessage("Invalid index")) *> stealFromPlayer(
                playerID,
                players,
                isRandom
              )
          }
        case None =>
          webSocketHub.sendToPlayer(playerID, colorErrorMessage("Invalid input")) *> stealFromPlayer(
            playerID,
            players,
            isRandom
          )
      }
    } yield ()

  private def stealCard(fromID: PlayerID, toID: PlayerID, cardOption: Option[Card]): IO[Unit] =
    webSocketHub.broadcast(s"$toID is stealing a card from $fromID") *> {
      gameStateRef
        .modify { gameState =>
          val hands = gameState.playersHands

          (hands.get(toID), hands.get(fromID)) match {
            case (_, None) => (gameState, None)
            case (Some(from), Some(to)) =>
              val index = cardOption.fold({
                println("here")
                Random.nextInt(from.length)}
              )(card => from.indexOf(card))

              println(index)
              from.get(index) match {
                case Some(card) =>
                  val newTo    = to.appended(card)
                  val newFrom  = from.filterNot(_ == card)
                  val newHands = hands + (fromID -> newFrom) + (toID -> newTo)
                  (gameState.copy(playersHands = newHands), Some(card))

                case None => (gameState, None)
              }

          }

        }
        .flatMap { optCard =>
          optCard.fold(webSocketHub.broadcast(s"$toID couldn't steal card from $fromID"))(card =>
            webSocketHub.broadcast(s"$toID stole a card from $fromID") *> webSocketHub.sendToPlayer(
              toID,
              s"Stole $card from $fromID"
            ) *> webSocketHub.sendToPlayer(fromID, s"$toID stole your $card")
          )
        }
    }

  private def pickFromPlayer(fromID: PlayerID, toID: PlayerID): IO[Card] = {
    val cardsPossible = List[Card](
      Defuse,
      Nope,
      Shuffle,
      Skip,
      AlterTheFuture3X,
      SwapTopAndBottom,
      Attack2X,
      TargetedAttack2X,
      CatomicBomb,
      Bury,
      Tacocat,
      FeralCat,
      Reverse
    )

    for {
      _ <- webSocketHub.sendToPlayer(toID, "What card do you want?")
      _ <- webSocketHub.sendToPlayer(
        toID,
        s"${cardsPossible.zipWithIndex.foldLeft("") { case (acc, (card, i)) =>
            acc ++ s"${i + 1}. $card       "
          }}"
      )
      _      <- webSocketHub.sendToPlayer(toID, "Insert index >> ")
      string <- webSocketHub.getGameInput(toID).map(_.toIntOption)
      card <- string match {
        case Some(index) =>
          index match {
            case i if (1 until cardsPossible.length) contains i =>
              cardsPossible(i - 1).pure[IO]
            case _ =>
              webSocketHub.sendToPlayer(toID, colorErrorMessage("Invalid choice")) *> pickFromPlayer(fromID, toID)
          }
        case None =>
          webSocketHub.sendToPlayer(toID, colorErrorMessage("Invalid input")) *> pickFromPlayer(fromID, toID)
      }
    } yield card
  }

  private def stealSpecificCard(fromID: PlayerID, toID: PlayerID): IO[Unit] =
    webSocketHub.broadcast(s"$toID is stealing a random card from $fromID") *> {
      gameStateRef
        .modify { gameState =>
          val hands = gameState.playersHands

          (hands.get(toID), hands.get(fromID)) match {
            case (None, _) | (_, None) => (gameState, None)
            case (Some(from), Some(to)) =>
              val index = Random.nextInt(from.length)
              from.get(index) match {
                case Some(card) =>
                  val newTo    = to.appended(card)
                  val newFrom  = from.filterNot(_ == card)
                  val newHands = hands + (fromID -> newFrom) + (toID -> newTo)
                  (gameState.copy(playersHands = newHands), Some(card))

                case None => (gameState, None)
              }

          }

        }
        .flatMap { optCard =>
          optCard.fold(webSocketHub.broadcast(s"$toID couldn't steal card from $fromID"))(card =>
            webSocketHub.broadcast(s"$toID stole a card from $fromID") *> webSocketHub.sendToPlayer(
              toID,
              s"Stole $card from $fromID"
            ) *> webSocketHub.sendToPlayer(fromID, s"$toID stole your $card")
          )
        }
    }
  private def handleCardPlayed(player: Player, ioCard: IO[Card]): IO[Boolean] =
    for {
      card <- ioCard
      _    <- webSocketHub.broadcast(colorPlayerMessage(player, s" played $card"))
      res  <- askIfNopeCards(player.playerID)
      skipped <-
        if (res) false.pure[IO] // Card was noped, card played with no effect
        else
          card match {
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
                _    <- buryCard(player.playerID, card)
              } yield true

            case Reverse =>
              updateDrawDeck(_.reversed) *> false.pure[IO]

            case Tacocat | FeralCat => false.pure[IO]

          }
    } yield skipped

  private def askIfNopeCards(playerID: PlayerID): IO[Boolean] = {
    for {
      playerHands <- gameStateRef.get.map(_.playersHands - playerID)
      nopeResult <- playerHands.toList.foldLeft(false.pure[IO]) { case (acc, (playerHandId, hand)) =>
        acc.flatMap(bool =>
          if (bool) true.pure[IO]
          else if (hand.contains(Nope))
            for {
              res <- askForNope(playerHandId, hand.indexOf(Nope))
            } yield res
          else IO.pure(false)
        )
      }
    } yield nopeResult
  }

  private def askForNope(playerID: PlayerID, index: Int): IO[Boolean] =
    for {
      _      <- webSocketHub.sendToPlayer(playerID, s"$playerID, do you wish to nope this action? (y/n)")
      answer <- webSocketHub.getGameInput(playerID)
      result <- answer match {
        case "y" => playCard(index) *> webSocketHub.broadcast(s"$playerID played $Nope") *> true.pure[IO]
        case _   => false.pure[IO]
      }
    } yield result

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

    val initialState = State(initialDrawDeck, initialDiscardDeck, currentPlayerIndex, playersList, Map.empty, Map.empty)

    for {
      stateManager <- StateManager.of(initialState, webSocketHub)
      gameStateRef <- Ref.of[IO, State](initialState)
    } yield new Game(nPlayers, webSocketHub, gameStateRef, stateManager)
  }
}
