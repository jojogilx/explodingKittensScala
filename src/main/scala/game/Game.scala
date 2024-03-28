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

import scala.concurrent.duration.DurationInt
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
  private def setRandomPlayerNext(): IO[Player] =
    gameStateRef
      .modify { gameState =>
        val index = Random.nextInt(gameState.players.length)
        (gameState.copy(currentPlayerIndex = index), gameState.players(index))
      }
      .flatTap(player => webSocketHub.broadcast(colorPlayerMessage(player, "'s starting\n")))

  /** Sets the current player index to the next player
    */
  private def rightOfPlayer(): IO[Player] =
    gameStateRef.modify { gameState =>
      val index = gameState.currentPlayerIndex + 1 match {
        case x if (1 until gameState.players.length).contains(x) => x
        case _                                                   => 0
      }
      (gameState.copy(currentPlayerIndex = index), gameState.players(index))
    }

  /** Sets the current player index to the previous player
    */
  private def leftOfPlayer(): IO[Player] =
    gameStateRef.modify { gameState =>
      val index = gameState.currentPlayerIndex - 1 match {
        case x if x < 0 => gameState.players.length - 1
        case x          => x
      }

      (gameState.copy(currentPlayerIndex = index), gameState.players(index))
    }

  private def nextPlayer(): IO[Player]= {
    gameStateRef.get.flatMap(gameState =>
      if(gameState.orderRight) rightOfPlayer() else leftOfPlayer()
    )
  }

  private def previousPlayer(): IO[Player]= {
    gameStateRef.get.flatMap(gameState =>
      if(gameState.orderRight) leftOfPlayer() else rightOfPlayer()
    )
  }

  private def reverseOrder(): IO[Unit] = {
    gameStateRef.update(gameState => gameState.copy(orderRight = !gameState.orderRight))
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

  // ----manage win and lose----------------------------------//

  /** Kills the current player, removing them from the game
    */
  private def killCurrentPlayer: IO[Unit] =
    gameStateRef
      .modify { gameState =>
        val currentIndex  = gameState.currentPlayerIndex
        val (left, right) = gameState.players.splitAt(currentIndex)
        val newPlayers    = left ::: right.drop(1)
        val newHands = gameState.playersHands - gameState.players(currentIndex).playerID

        val index = currentIndex - 1 match {
          case x if x < 0 => gameState.players.length - 1
          case x          => x
        }

        (gameState.copy(discardDeck= gameState.discardDeck.prepend(ExplodingKitten),players = newPlayers, currentPlayerIndex = index, playersHands = newHands), right.head) // > avoid head

      }
      .flatMap(player => webSocketHub.broadcast(diedMessage(player)))

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

  // -------------game loop---------------------//

  /** Sends the game title, hands cards, sets a random starting player and starts the game loop
    */
  def initialize(): IO[Unit] = {
    for {
      _ <- webSocketHub.broadcast(gameTitleBanner)
      _ <- webSocketHub.broadcast(colorSystemMessage(s"Initializing..."))
      _ <- handCards()
      player <- setRandomPlayerNext()
      _ <- gameLoop(player)
    } yield ()
  }

  /** The main game loop, starts a turn then updates the next player, stops when there's a winner at the end of a turn
    */
  private def gameLoop(player: Player): IO[Unit] = {
    playerTurn(player) *> getWinner.flatMap({
      case Some(player) =>
        webSocketHub.broadcast(
          colorPlayerMessage(player, s" won the game $PartyEmojiUnicode$PartyEmojiUnicode")
        ) *> webSocketHub.endGame() *> IO.println(s"ended")
      case None => nextPlayer().flatMap(np => gameLoop(np))
    })
  }

  /** Handles a player turn
    */ // meter a player turn com o id do player e quantas turns tem de fazer, then do it recursively
  private def playerTurn(currentPlayer: Player): IO[Unit] = IO
    .race( // On current player disconnected - break
      for {
        gameState <- gameStateRef.get
        playerID = currentPlayer.playerID
        deferred <- IO.fromOption(gameState.disconnections.get(playerID))(new RuntimeException("Deferred not found"))
        _        <- deferred.get
      } yield (),
      for {
        _ <- webSocketHub.broadcast(s"\n$TurnSeparator\n")

        gameState <- gameStateRef.get
        playerID = currentPlayer.playerID

        _ <- IO.println(s"discard: ${gameState.discardDeck}") //
        _ <- IO.println(s"draw: ${gameState.drawDeck}")       //

        _    <- webSocketHub.broadcast(colorPlayerMessage(currentPlayer, "'s turn"))
        hand <- getHandWithIndex(playerID)
        _    <- webSocketHub.sendToPlayer(playerID, s"Your hand is: $hand")

        playOrPass <-  if(canPlayAnything(gameState.playersHands(playerID)))
          playOrPassPrompt(currentPlayer) // Does player want to play a card?
        else IO(None)
        playerSkipped <- playOrPass.fold(false.pure[IO])(i => {
          handleCardPlayed(currentPlayer, playCard(i))
        }) // If card played, does player skip draw?
        _ <-
          if (!playerSkipped) {
            for {
              card <- drawCard()
              _    <- webSocketHub.sendToPlayer(playerID, s"$card drawn")
              _ <- card match {
                case ExplodingKitten =>
                  for {
                    _         <- webSocketHub.broadcast(s"$playerID drew $card")
                    defuseOpt <- tryFindDefuseIndex()
                    _ <- defuseOpt.fold(killCurrentPlayer)(index =>
                      for{
                        _ <- playCard(index)
                        _ <-webSocketHub.broadcast(s"$Defuse used")
                        _ <- webSocketHub.sendToPlayer(playerID, s"Choose where to bury the $ExplodingKitten")
                        _ <- buryCard(playerID, ExplodingKitten)
                      } yield ()
                    )
                  } yield ()
                case _ => IO.unit
              }
            } yield ()
          } else IO.unit

      } yield ()
    )
    .void

  private def canPlayAnything(hand: Player.Hand): Boolean = {
    hand.nonEmpty && !hand.forall(_ match {
      case ExplodingKitten | Defuse | Nope => true
      case _ => false
    })
  }

  /** Prompt that asks if and which card the player wants to play
    * @param player
    *   current player
    * @return
    *   optional of the index of card played
    */
  private def playOrPassPrompt(player: Player): IO[Option[Int]] =
    for {
      state <- gameStateRef.get
      _ <- webSocketHub.broadcast(colorSystemMessage(s"Draw pile size: ${state.drawDeck.length} cards${state.discardDeck.topCard.fold("")(card => s", last played card was $card")}"))
      _ <- webSocketHub.sendToPlayer(player.playerID, colorPlayerMessage(player, s", do you wish to play a card?"))
      playerHand = state.playersHands(player.playerID)

      _ <- webSocketHub.sendToPlayer(
        player.playerID,
        "Enter the index of the card you want to play (n to Pass or index -h to get card description) (to use cat cards combo: e.g. 1,2 or 1,2,3)>> "
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
                player
              )
            case _ =>
              webSocketHub.sendToPlayer(
                player.playerID,
                colorErrorMessage("Invalid index (e.g.: 1 -h)")
              ) *> playOrPassPrompt(
                player
              )
          }
        case s"${c1},${c2}" if c1.toIntOption.isDefined && c2.toIntOption.isDefined =>
          {
            List(c1.toInt - 1, c2.toInt - 1) match {
              case list if list.forall(i => {
                    playerHand.indices.contains(i) && (playerHand(i) match {
                      case Tacocat | FeralCat => true
                      case _                  => false
                    })
                  }) =>
                playCards(list) *> stealFromPlayer(
                  player.playerID,
                  state.players,
                  isRandom = true
                )
              case _ =>
                webSocketHub.sendToPlayer(
                  player.playerID,
                  "Invalid input, play 2 indices of cat cards (e.g.: 1,2)"
                ) *> playOrPassPrompt(player)
            }
          } *> None.pure[IO]
        case s"${c1},${c2},${c3}" if c1.toIntOption.isDefined && c2.toIntOption.isDefined && c3.toIntOption.isDefined =>
          {
            List(c1.toInt - 1, c2.toInt - 1, c3.toInt - 1) match {
              case list if list.forall(i => {
                    playerHand.indices.contains(i) && (playerHand(i) match {
                      case Tacocat | FeralCat => true
                      case _                  => false
                    })
                  }) =>
                playCards(list) *> stealFromPlayer(
                  player.playerID,
                  state.players,
                  isRandom = false
                )
              case _ =>
                webSocketHub.sendToPlayer(
                  player.playerID,
                  "Invalid input, play 3 indices of cat cards (e.g.: 1,2,3)"
                ) *> playOrPassPrompt(player)
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
                  ) *> playOrPassPrompt(player)
                case _ => Some(i).pure[IO]
              }

            case _ =>
              webSocketHub.sendToPlayer(player.playerID, colorErrorMessage("Invalid index")) *> playOrPassPrompt(
                player
              )
          }

        case _ =>
          webSocketHub.sendToPlayer(player.playerID, colorErrorMessage("Invalid input")) *> playOrPassPrompt(
            player
          )
      }
    } yield result

  /** Handles the card played, performing the necessary operations
    * @param player
    *   current player
    * @param ioCard
    *   card played
    * @return
    *   if the played card caused the player to skip drawing a card
    */
  private def handleCardPlayed(player: Player, ioCard: IO[Card]): IO[Boolean] =
    for {
      card <- ioCard
      _    <- webSocketHub.broadcast(colorPlayerMessage(player, s" played $card"))
      res  <- askIfNopeCards2(player.playerID)
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
                np <- nextPlayer()
                _ <- playerTurn(np)
                _ <- previousPlayer()
              } yield true

            case TargetedAttack2X =>
              for {
                nextPlayer <- targetAttack(player.playerID)
                _          <- setNextPlayer(nextPlayer)
                _          <- playerTurn(nextPlayer)
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
              reverseOrder() *> true.pure[IO]

            case Tacocat | FeralCat => false.pure[IO]

            case BarkingKitten => ???
            case DrawFromTheBottom => ???
            case GarbageCollection => ???
            case IllTakeThat => ???
            case ImplodingKitten => ???
            case Mark => ???
            case PersonalAttack3X => ???
            case SeeTheFuture3X => ???
            case SeeTheFuture5X => ???
            case ShareTheFuture3X => ???
            case StreakingKitten => ???
            case SuperSkip => ???
          }
    } yield skipped

  // ___________ Deck Operations ______________________________//

  /** Draws a card from the draw deck, if no cards are available, piles are switched and shuffled If the card is a
    * exploding kitten it's discarded, otherwise it's added to the current player's hand
    * @return
    *   the card drawn
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
            (gameState.copy(drawDeck = deck), card)
          case _ =>
            (
              gameState.copy(drawDeck = deck, playersHands = gameState.playersHands + (currentPlayer -> playerHand)),
              card
            )

        }
      }
    } yield card

  /** Adds the remaining cards from the draw deck and the discard deck and shuffles them into a new draw deck. Clears
    * the discard deck
    */
  private def switchPiles2(): IO[Unit] =
    webSocketHub.broadcast(colorSystemMessage(s"Switching piles...\n")) *>
      gameStateRef.update { gameState =>
        val draw = Deck.initShuffledFromDiscardPile2(gameState.drawDeck, gameState.discardDeck)
        gameState.copy(drawDeck = draw, discardDeck = Deck(List.empty))
      }

  /** Removes all bombs and defuses from the deck and deals the players 7 cards + a defuse then adds the bombs back and
    * shuffles the deck
    */
  private def handCards(): IO[Unit] = {
    webSocketHub.broadcast(colorSystemMessage(s"Handing cards...\n")) *> {
      gameStateRef.update { gameState =>
        val (deckWOBombs, bombs) = removeDefuseAndBombs(gameState.drawDeck, NopeSauce.defusesOnStart * nPlayers)
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

  /** Updates the draw deck according to a given function
    * @param function
    *   function to apply on the deck
    */
  private def updateDrawDeck(function: Deck => Deck): IO[Unit] =
    gameStateRef.update { gameState =>
      gameState.copy(drawDeck = function(gameState.drawDeck))
    }

  // ------ players hand--------------------------------//

  /** Tries to find a defuse in the cards the player is holding
    *
    * @return
    *   Option of defuse index in the current player's hand
    */
  private def tryFindDefuseIndex(): IO[Option[Int]] =
    gameStateRef.get.map { gameState =>
      val playerID = gameState.players(gameState.currentPlayerIndex).playerID
      val hand     = gameState.playersHands.getOrElse(playerID, List.empty)
      hand.zipWithIndex
        .collectFirst { case (Defuse, i) => i }
    }

  /** Plays the card at given index, removing it from the player's hand, discarding it and returning the card
    *
    * @param index
    *   the index of the card to play
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

  /** Plays the cards at given indices, removing them from the player's hand, discarding them
   *
   * @param indices
   *   the indices of the card to play
   * @return
   *   Card played
   */
  private def playCards(indices: List[Int]): IO[Unit] =
    gameStateRef.update { gameState =>
      val currentPlayer = gameState.players(gameState.currentPlayerIndex)
      val hands         = gameState.playersHands



      val (newHand, discard) = hands(currentPlayer.playerID).zipWithIndex.partition {
        case (_, i) if indices.contains(i) => false
        case _ => true
      }

      println(s"\nplayed $newHand")
      println(s"\ndiscarded $discard")

        gameState.copy(
          discardDeck = gameState.discardDeck.concat(discard.map(_._1)),
          playersHands = hands + (currentPlayer.playerID -> newHand.map(_._1))
        )

    }

  /** Plays the card with the given player at given index, removing it from the player's hand, discarding it and
    * returning the card
    * @param playerID
    *   id of the player
    * @param index
    *   the index of the card to play
    * @return
    *   Card played
    */
  private def playCardByID(playerID: PlayerID, index: Int): IO[Card] =
    gameStateRef.modify { gameState =>
      val player        = gameState.players.filter(_.playerID == playerID).head
      val hands         = gameState.playersHands
      val card          = hands(player.playerID)(index)
      val (left, right) = hands(player.playerID).splitAt(index)
      val newCards      = left ::: right.drop(1)

      (
        gameState.copy(
          discardDeck = gameState.discardDeck.prepend(card),
          playersHands = hands + (player.playerID -> newCards)
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
        .map {
          case (card, i) => s"${i + 1}. $card   "
        }
        .mkString
    }

  // -----------prompts cards-----------------//

  /** Prompt for the alter the future card, players are asked to order the next 3 cards
    * @param cards3
    *   the next 3 cards
    * @param playerID
    *   the current player's id
    * @return
    *   the new order of the cards, chosen by the player
    */
  private def alterTheFuture(cards3: List[Card], playerID: PlayerID): IO[String] = {
    for {
      _ <- webSocketHub.sendToPlayer(playerID, "Next three cards are: \n")
      _ <- webSocketHub.sendToPlayer(
        playerID,
        getStringWithIndex(cards3)
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

  /** Prompt for Bury card. Asks player where they want to bury card and when valid, inserts the card at that index
    * @param playerID
    *   current player's id
    * @param card
    *   card to bury (hidden from the player)
    */
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

  /** Prompt for Targeted attack, asks player who they want to target
    * @param playerID
    *   the current player's id
    * @return
    *   the player chosen to be attacked
    */
  private def targetAttack(playerID: PlayerID): IO[Player] =
    for {
      players <- gameStateRef.get.map(_.players.filterNot(_.playerID == playerID))
      _       <- webSocketHub.sendToPlayer(playerID, "\nWho do you want to target? \n")
      _ <- webSocketHub.sendToPlayer(
        playerID,
        getStringWithIndex(players.map(_.playerID))
      )
      _      <- webSocketHub.sendToPlayer(playerID, "Insert index >> ")
      string <- webSocketHub.getGameInput(playerID).map(_.toIntOption)
      valid <- string match {
        case Some(value) =>
          value match {
            case x if (1 to players.length) contains x =>
              players.filterNot(_.playerID == playerID)(x - 1).pure[IO]
            case _ =>
              webSocketHub.sendToPlayer(playerID, colorErrorMessage("Invalid index")) *> targetAttack(playerID)
          }
        case None =>
          webSocketHub.sendToPlayer(playerID, colorErrorMessage("Invalid input")) *> targetAttack(playerID)
      }
    } yield valid

  /** Prompt for choosing a player to steal a card from, then steals it either randomly or asks the player to name a
    * card
    * @param playerID
    *   current player's id
    * @param players
    *   list of current players
    * @param isRandom
    *   if the card stolen should be random
    */
  private def stealFromPlayer(playerID: PlayerID, players: List[Player], isRandom: Boolean): IO[Unit] =
    for {
      _ <- webSocketHub.sendToPlayer(playerID, "Who do you want to steal from?")
      playersFilter = players.filterNot(_.playerID == playerID)
      _ <- webSocketHub.sendToPlayer(
        playerID,
        getStringWithIndex(playersFilter.map(_.playerID))
      )
      _      <- webSocketHub.sendToPlayer(playerID, "Insert index >> ")
      string <- webSocketHub.getGameInput(playerID).map(_.toIntOption)
      _ <- string match {
        case Some(value) =>
          value match {
            case x if (1 to playersFilter.length) contains x =>
              if (isRandom)
                stealCard(fromID = playersFilter(x - 1).playerID, toID = playerID, None)
              else
                pickFromPlayer(
                  fromID = playersFilter(x - 1).playerID,
                  toID = playerID
                ).flatMap(card => stealCard(fromID = playersFilter(x - 1).playerID, toID = playerID, Some(card)))
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

  /** Tries to steal a card from a given player
    * @param fromID
    *   id of the player to steal from
    * @param toID
    *   if of the player to receive the card
    * @param cardOption
    *   option of a chosen card to steal
    */
  private def stealCard(fromID: PlayerID, toID: PlayerID, cardOption: Option[Card]): IO[Unit] =
    (if(cardOption.isDefined) webSocketHub.broadcast(s"$toID is trying to steal a ${cardOption.get} from $fromID") else webSocketHub.broadcast(s"$toID is stealing a card from $fromID")) *> {
      gameStateRef
        .modify { gameState =>
          val hands = gameState.playersHands

          (hands.get(toID), hands.get(fromID)) match {
            case (_, None) => (gameState, None)
            case (Some(to), Some(from)) =>
              val index = cardOption.fold({
                Random.nextInt(from.length)
              })(card => from.indexOf(card))


              from.get(index) match {
                case Some(card) =>
                  val newTo    = to.appended(card)

                  val (a,b)  = from.splitAt(index)
                  val newFrom = a ++ b.drop(1)

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

  /** Prompt for choosing a card type to steal
    * @param fromID
    *   id of the player to steal from
    * @param toID
    *   id of the player stealing
    * @return
    *   the chosen card type to steal
    */
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
        getStringWithIndex(cardsPossible)
      )
      _      <- webSocketHub.sendToPlayer(toID, "Insert index >> ")
      string <- webSocketHub.getGameInput(toID).map(_.toIntOption)
      card <- string match {
        case Some(index) =>
          index match {
            case i if (1 to cardsPossible.length) contains i =>
              cardsPossible(i - 1).pure[IO]
            case _ =>
              webSocketHub.sendToPlayer(toID, colorErrorMessage("Invalid choice")) *> pickFromPlayer(fromID, toID)
          }
        case None =>
          webSocketHub.sendToPlayer(toID, colorErrorMessage("Invalid input")) *> pickFromPlayer(fromID, toID)
      }
    } yield card
  }

  /** they want to nope this action, returns true on the first player that answers yes
    * @param playerID
    *   current player's id
    * @return
    *   true if anyone noped
    */

  private def askIfNopeCards2(playerID: PlayerID): IO[Boolean] = {
    for {
      playerHandsWithNope <- gameStateRef.get
        .map(_.playersHands - playerID)
        .map(_.filter {
          case (_, hand) if hand.contains(Nope) => true
          case _                                => false
        })
      result <-
        IO.race(
          if (playerHandsWithNope.isEmpty) IO.unit
          else
            webSocketHub.broadcast(colorSystemMessage("Checking if action is noped")) *> IO.sleep(
              100.millis
            ) *> broadCastCountDown(5) *> false.pure[IO],
          for {
            _ <- playerHandsWithNope.keys.toList.parTraverse { pID =>
              webSocketHub.sendToPlayer(pID, s"$pID, do you wish to nope this action? (y/n)")
            }
            ans <- webSocketHub.getPendingInputs(playerHandsWithNope.keys.toList, message = "y")
            res <- ans.fold(false.pure[IO]) { pID =>
              playCardByID(pID, playerHandsWithNope(pID).indexOf(Nope)) *> webSocketHub.broadcast(
                s"$pID played $Nope"
              ) *> true.pure[IO]
            }
          } yield res
        ).flatMap {
          case Left(_)      => false.pure[IO]
          case Right(value) => value.pure[IO]
        }
    } yield result
  }

  private def broadCastCountDown(counter: Int): IO[Unit] =
    if (counter <= 0) IO.unit
    else
      for {
        _ <- webSocketHub.broadcast(colorSystemMessage(s"$counter"))
        _ <- IO.sleep(1.seconds)
        _ <- broadCastCountDown(counter - 1)
      } yield ()

}
object Game {

  /** Creates a new game for a number of players and a websockethub
    * @param nPlayers
    *   number of players
    * @param webSocketHub
    *   websockethub required for communication with game and players
    * @return
    *   game created
    */
  def create(
      nPlayers: Int,
      webSocketHub: WebSocketHub
  ): IO[Game] = {
    val initialDrawDeck    = initFromRecipe(NopeSauce,nPlayers)
    val initialDiscardDeck = Deck(List.empty)
    val currentPlayerIndex = -1
    val playersList        = List.empty

    val initialState = State(initialDrawDeck.get, initialDiscardDeck, currentPlayerIndex, playersList, Map.empty, Map.empty, orderRight = true)

    for {
      stateManager <- StateManager.of(initialState, webSocketHub)
      gameStateRef <- Ref.of[IO, State](initialState)
    } yield new Game(nPlayers, webSocketHub, gameStateRef, stateManager)
  }
}
