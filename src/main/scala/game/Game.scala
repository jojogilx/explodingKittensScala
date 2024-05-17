package game

import card.Deck._
import card._
import cats.effect.{Deferred, IO, Ref}
import cats.implicits._
import gamestate._
import players.Player
import players.Player.{Hand, HandCount, PlayerID}
import utils.utils._
import websockethub.Event._
import websockethub.{PromptsHandler, WebSocketHub}

import scala.concurrent.duration.DurationInt
import scala.util.Random

case class Game(
    roomName: String,
    webSocketHub: WebSocketHub,
    gameStateRef: Ref[IO, State],
    // stateManager: StateManager,
    prompter: PromptsHandler,
    recipe: Recipe,
    started: Deferred[IO, Boolean]
) {

  /** Creates a player with PlayerID and joins the game
    * @param playerID
    *   id of the player
    */
  def join(playerID: PlayerID): IO[Either[String, Unit]] =
    for {
      state <- gameStateRef.get
      isAlreadyConnected = state.players.contains(Player(playerID, _))
      cantJoin           = state.started || (recipe.maxPlayers < state.players.length + 1)
      res <-
        if (cantJoin) IO(Left("Can't Join"))
        else if (isAlreadyConnected) {
          for {
            _ <- reconnect(playerID)
            _ <- webSocketHub.sendToPlayer2(playerID)(RoomStateEvent(state.players, recipe))
          } yield Right()
        } else {
          for {
            deferred <- Deferred[IO, Boolean]
            newState <- gameStateRef.updateAndGet { gameState =>
              val disconnectedPlayers = gameState.disconnections + (playerID -> deferred)
              val newPlayer           = Player(playerID, gameState.players.length)
              val updatedPlayers      = newPlayer :: gameState.players
              gameState.disconnections + (playerID -> Deferred)
              gameState.copy(players = updatedPlayers, disconnections = disconnectedPlayers)
            }
            _ <- webSocketHub.broadcast(Joined(playerID, newState.players))
            _ <- webSocketHub.sendToPlayer2(playerID)(RoomStateEvent(newState.players, recipe))
          } yield Right()
        }
    } yield res

  private def initialize(): IO[Unit] = {
    gameStateRef.get.flatMap { gameState =>
      if (gameState.started) {
        IO.unit
      } else {
        for {
          _        <- webSocketHub.broadcast(Started())
          _        <- webSocketHub.broadcast(Information("Initializing"))
          nPlayers <- IO(gameState.players.length)
          drawDeck = initFromRecipe(recipe, nPlayers)
          _      <- gameStateRef.update(_.copy(drawDeck = drawDeck))
          _      <- handCards(nPlayers)
          hands  <- getAllShownCardsInHands
          _      <- webSocketHub.broadcast(PlayersHands(hands.toList))
          player <- setRandomPlayerNext()
          _      <- gameStateRef.get.map(state => webSocketHub.broadcast(PilesUpdate(state.drawDeck.length)))
          _      <- gameLoop(player).start
        } yield ()
      }
    }
  }

  /** Starts the game
    * @return
    *   IO with either error string or unit
    */
  def start(): IO[Either[String, Unit]] =
    for {
      state <- gameStateRef.get
      res <-
        if (state.started)
          Left("Game already started").pure[IO]
        else initialize() *> IO(Right())
    } yield res

  private def reconnect(playerID: PlayerID): IO[Unit] = {
    for {
      handOpt <- getHand(playerID)
      _ <- handOpt.fold(IO.println("recon failed to update hand"))(hand =>
        webSocketHub.sendToPlayer2(playerID)(CardsInHand(hand))
      )
    } yield ()
  }

  /** Callback that warns the game the player disconnected
    * @param playerID
    *   player disconnected
    */
  def playerDisconnected(playerID: PlayerID): IO[Unit] =
    IO.println(s"$playerID disconnected from game") *> previousPlayer() *> {
      for {
        gameState <- gameStateRef.get
        deferred  <- IO.fromOption(gameState.disconnections.get(playerID))(new RuntimeException("Deferred not found"))
        _         <- webSocketHub.disconnectPlayer(playerID)

        _ <- deferred.complete(true)
      } yield ()
    } *> gameStateRef
      .modify(gameState => {
        val newPlayers = gameState.players.filterNot(_.playerID == playerID)

        (gameState.copy(players = newPlayers), newPlayers)
      })
      .flatMap(players => webSocketHub.broadcast(LeftGame(playerID, players)))

  // -----manage player turns -------------------------------//

  /** Sets the current player index to a random number, bounded by how many players are in-game
    */
  private def setRandomPlayerNext(): IO[PlayerID] =
    webSocketHub.broadcast(Information("Selecting a random player to start...")) *> gameStateRef
      .modify { gameState =>
        val index = Random.nextInt(gameState.players.length)
        (gameState.copy(currentPlayerIndex = index, turnsLeft = 1), gameState.players(index).playerID)
      }
      .flatTap(player => webSocketHub.broadcast(Information(s"$player's starting")))

  /** Sets the current player index to the next player
    */
  private def rightOfPlayer(): IO[PlayerID] =
    gameStateRef.modify { gameState =>
      val index = gameState.currentPlayerIndex + 1 match {
        case x if (1 until gameState.players.length).contains(x) => x
        case _                                                   => 0
      }
      (gameState.copy(currentPlayerIndex = index), gameState.players(index).playerID)
    }

  /** Sets the current player index to the previous player
    */
  private def leftOfPlayer(): IO[PlayerID] =
    gameStateRef.modify { gameState =>
      val index = gameState.currentPlayerIndex - 1 match {
        case x if x < 0 => gameState.players.length - 1
        case x          => x
      }

      (gameState.copy(currentPlayerIndex = index), gameState.players(index).playerID)
    }

  /** gets the current player index to the next player
    */
  private def getRightOfPlayer: IO[PlayerID] =
    gameStateRef.get.map { gameState =>
      val index = gameState.currentPlayerIndex + 1 match {
        case x if (1 until gameState.players.length).contains(x) => x
        case _                                                   => 0
      }
      gameState.players(index).playerID
    }

  /** gets the current player index to the previous player
    */
  private def getLeftOfPlayer: IO[PlayerID] =
    gameStateRef.get.map { gameState =>
      val index = gameState.currentPlayerIndex - 1 match {
        case x if x < 0 => gameState.players.length - 1
        case x          => x
      }
      gameState.players(index).playerID
    }

  private def nextPlayer(): IO[PlayerID] = {
    gameStateRef.update(state => state.copy(turnsLeft = 1)) *> gameStateRef.get.flatMap(gameState =>
      if (gameState.orderRight) rightOfPlayer() else leftOfPlayer()
    )
  }

  private def getNextPlayer: IO[PlayerID] = {
    gameStateRef.get.flatMap(gameState => if (gameState.orderRight) getRightOfPlayer else getLeftOfPlayer)
  }

  private def previousPlayer(): IO[PlayerID] = {
    gameStateRef.get.flatMap(gameState => if (gameState.orderRight) leftOfPlayer() else rightOfPlayer())
  }

  private def reverseOrder(): IO[Unit] = {
    gameStateRef.update(gameState => gameState.copy(orderRight = !gameState.orderRight))
  }

  /** Sets the current player index to the index of a given player
    * @param player
    *   player to set as the next one
    */
  private def setNextPlayer(playerID: PlayerID): IO[Unit] =
    gameStateRef.update { gameState =>
      val index  = gameState.players.map(_.playerID).indexOf(playerID)
      gameState.copy(currentPlayerIndex = index)
    }

  // ----manage win and lose----------------------------------//

  /** Kills the current player, removing them from the game
    */
  private def killPlayer(playerID: PlayerID, card: Card): IO[Unit] =
    gameStateRef
      .update { gameState =>
        val currentIndex  = gameState.players.map(_.playerID).indexOf(playerID)
        val (left, right) = gameState.players.splitAt(currentIndex)
        val newPlayers    = left ::: right.drop(1)
        val newHands      = gameState.playersHands - gameState.players(currentIndex).playerID

        val index = currentIndex - 1 match {
          case x if x < 0 => gameState.players.length - 1
          case x          => x
        }


          gameState.copy(
            discardDeck = gameState.discardDeck.prepend(card),
            players = newPlayers,
            currentPlayerIndex = index,
            playersHands = newHands
          )


      }
      .flatMap(player => webSocketHub.broadcast(s" $playerID DIED"))

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

  /** The main game loop, starts a turn then updates the next player, stops when there's a winner at the end of a turn
    */
  private def gameLoop(player: PlayerID): IO[Unit] = {
    playerTurn(player) *> getWinner.flatMap({
      case Some(player) =>
        webSocketHub.broadcast(Winner(player.playerID)) *> webSocketHub.endGame() *> IO.println(s"ended")
      case None => nextPlayer().flatMap(np => gameLoop(np)) // todo: set currentPlayer in playerturn
    })
  }

  private def debugTurn(): IO[Unit] = for {
    gameState <- gameStateRef.get
    _         <- IO.println(s"discard: ${gameState.discardDeck}")
    _         <- IO.println(s"draw: ${gameState.drawDeck}")
  } yield ()

  private def playerOnDisconnect(playerID: PlayerID): IO[Unit] =
    for {
      gameState <- gameStateRef.get
      deferred  <- IO.fromOption(gameState.disconnections.get(playerID))(new RuntimeException("Deferred not found"))
      _         <- deferred.get
    } yield ()

  /** Handles a player turn
    */
  private def playerTurn(playerID: PlayerID): IO[Unit] = IO
    .race( // On current player disconnected - break
      playerOnDisconnect(playerID),
      for {
        _ <- debugTurn()
        _ <- webSocketHub.broadcast(NewTurn(playerID))

        turns     <- gameStateRef.get.map(_.turnsLeft)
        _         <- playCardsLoop(playerID, turns)
        turnsLeft <- gameStateRef.get.map(_.turnsLeft)
        _ <-
          if (turnsLeft > 0) {
            for {
              _         <- handleDrawCard(playerID, top = true)
              gameState <- gameStateRef.get
              _ <- webSocketHub.broadcast(
                PilesUpdate(gameState.drawDeck.length)
              )

            } yield ()
          } else IO.unit

      } yield ()
    )
    .void

  private def playCardsLoop(playerID: PlayerID, turns: Int): IO[Unit] =
    (for {
      hand      <- getHand(playerID)
      turnsLeft <- gameStateRef.get.map(_.turnsLeft)
      pass <- hand
        .fold(true.pure[IO])(hand =>
          if (turnsLeft < turns) true.pure[IO]
          else if (canPlayAnything(hand))
            playOneCard(playerID, hand)
          else
            webSocketHub.sendToPlayer2(playerID)(Information("You don't have any cards you can play")) *> true.pure[IO]
        )
    } yield pass).flatMap(pass =>
      if (pass) {
        IO.unit
      } else {
        playCardsLoop(playerID, turns)
      }
    )

//return if player wants to play more
  private def playOneCard(playerID: PlayerID, hand: Hand): IO[Boolean] =
    for {
      playedCards <- prompter.playCardsPrompt(playerID, hand)
      res <- playedCards.fold(true.pure[IO])(list =>
        list
          .traverse_(i =>
            for {
              card    <- playCard(i, playerID)
              _       <- handleCardPlayed(playerID, card)
              handNew <- getHand(playerID)
              _       <- webSocketHub.sendToPlayer2(playerID)(CardsInHand(handNew.get))
            } yield ()
          )
          .flatMap(_ => false.pure[IO])
      )

    } yield res

  private def getHand(playerID: PlayerID): IO[Option[Hand]] =
    gameStateRef.get.map { state =>
      state.playersHands.find { case (pID, _) => pID == playerID }.map(_._2).map((_._1))
    }

  private def getAllShownCardsInHands: IO[Map[PlayerID, HandCount]] =
    gameStateRef.get.map { state =>
      state.playersHands.map { case (id, (_, count)) => (id, count) }
    }
  private def canPlayAnything(hand: Hand): Boolean =
    hand.nonEmpty && !hand.forall(_ match {
      case ExplodingKitten | Defuse | Nope => true
      case _                               => false
    })

  private def tryToDefuseCard(playerID: PlayerID, card: Card): IO[Unit] =
    for {
    _         <- webSocketHub.broadcast(Information(s"$playerID drew ${card.title}"))
    defuseOpt <- tryFindDefuseIndex()
    _ <- defuseOpt.fold(killPlayer(playerID, card))(
      index =>
        for {
          _ <- playCard(index, playerID)
          _ <- webSocketHub.broadcast(Information(s"$Defuse used"))
          _ <- buryCard(playerID, card)
        } yield ()
    )
  } yield ()

  /** Handles the card played, performing the necessary operations
    * @param player
    *   current player
    * @param card
    *   card played
    * @return
    *   if the played card caused the player to skip drawing a card
    */
  // TODO restructure to include cat cards
  private def handleCardPlayed(player: PlayerID, card: Card): IO[Unit] =
    for {
      _   <- webSocketHub.broadcast(s"$player played $card")
      res <- if (card == BarkingKitten) false.pure[IO] else askIfNopeCards2(player)
      _ <- card match {
        case SuperSkip  => gameStateRef.update(state => state.copy(turnsLeft = 0))
        case _: Skipper => gameStateRef.update(state => state.copy(turnsLeft = state.turnsLeft - 1))

        case _ => IO.unit
      }
      _ <-
        if (res) IO.unit // Card was noped, card played with no effect
        else
          card match {
            case Shuffle =>
              updateDrawDeck(_.shuffled)

            case AlterTheFuture3X =>
              for {
                cards3 <- gameStateRef.get.map(_.drawDeck.getFirstN(3))
                order  <- prompter.alterTheFuture(cards3, player)
                _      <- updateDrawDeck(_.alterTheFuture3X(order))
              } yield ()

            case SwapTopAndBottom =>
              updateDrawDeck(_.swapTopAndBottom)

            case Attack2X =>
              for {
                np <- nextPlayer()
                _  <- playerTurn(np)
                _  <- previousPlayer()
              } yield ()

            case TargetedAttack2X =>
              for {
                players <- gameStateRef.get.map(_.players.filterNot(_.playerID == player).map(_.playerID))
                nextPlayer <- prompter.choosePlayer(player, players)
                _          <- setNextPlayer(nextPlayer)
                _          <- playerTurn(nextPlayer)
                _          <- previousPlayer()
              } yield ()

            case CatomicBomb =>
              updateDrawDeck(_.withExplodingKittensOnTop)

            case Bury =>
              for {
                card <- handleDrawCard(player, top = true)
                _    <- buryCard(player, card)
                _    <- gameStateRef.update(state => state.copy(turnsLeft = state.turnsLeft - 1))
              } yield ()

            case Reverse =>
              reverseOrder()

            case BarkingKitten =>
              for {
                playersWithBarkingKitten <- gameStateRef.get.map(
                  _.playersHands.find { case (_, (hand, _)) => hand.contains(BarkingKitten) }.map(_._1)
                )
                _ <- playersWithBarkingKitten match {
                  case Some(player) => tryToDefuseCard(player, BarkingKitten)
                  case None         => gameStateRef.update(state => state.copy(barkingKitten = player.some))
                }
              } yield ()
            case DrawFromTheBottom =>
              handleDrawCard(player, top = false) *> gameStateRef.update(state =>
                state.copy(turnsLeft = state.turnsLeft - 1)
              )
            case GarbageCollection =>
              for {
                playersWithCards <- gameStateRef.get.map(_.playersHands.filter { case (_, h) => h.nonEmpty }.keys)
                _                <- prompter.garbageCollectionPrompt(playersWithCards.toList)
              } yield ()
            case IllTakeThat =>
              for {
                players      <- gameStateRef.get.map(_.players.filterNot(_.playerID == player))
                chosenPlayer <- prompter.choosePlayer(player, players.map(_.playerID))
                _ <- gameStateRef.update(state => state.copy(marking = state.marking :+ (player, chosenPlayer)))
                _ <- webSocketHub.broadcast(Information(s"$chosenPlayer's next card will be taken by $player'"))
              } yield ()
            case Mark =>
              for {
                players      <- gameStateRef.get.map(_.players.filterNot(_.playerID == player))
                chosenPlayer <- prompter.choosePlayer(player, players.map(_.playerID))
                opt          <- getHand(chosenPlayer)
                _            <- opt.fold(IO.unit)(hand => showCardFromPlayer(chosenPlayer, getRandomFromList(hand)))
              } yield ()
            case PersonalAttack3X => gameStateRef.update(state => state.copy(turnsLeft = state.turnsLeft + 2))
            case SeeTheFuture3X =>
              gameStateRef.get
                .map(_.drawDeck)
                .flatMap(drawDeck =>
                  webSocketHub.sendToPlayer2(player)(Information(s"Next 3 cards are ${drawDeck.getFirstN(3)}"))
                )
            case SeeTheFuture5X =>
              gameStateRef.get
                .map(_.drawDeck)
                .flatMap(drawDeck =>
                  webSocketHub.sendToPlayer2(player)(Information(s"Next 5 cards are ${drawDeck.getFirstN(5)}"))
                )
            case ShareTheFuture3X =>
              for {
                cards3 <- gameStateRef.get.map(_.drawDeck.getFirstN(3))
                order  <- prompter.alterTheFuture(cards3, player)
                _      <- updateDrawDeck(_.alterTheFuture3X(order))
                newCards   <- gameStateRef.get.map(_.drawDeck.getFirstN(3))
                nextPlayer <- getNextPlayer
                _          <- webSocketHub.sendToPlayer2(nextPlayer)(SeeCards(newCards))
              } yield ()

            case _ => IO.unit
          }
    } yield ()

  // ___________ Deck Operations ______________________________//

  private def showCardFromPlayer(playerID: PlayerID, card: Card): IO[Unit] =
    for {
      _ <- gameStateRef.update(state => {
        val (hand, HandCount(hidden, shown)) = state.playersHands(playerID)
        state.copy(playersHands = state.playersHands + (playerID -> (hand, HandCount(hidden - 1, shown :+ card))))
      })
      _ <- webSocketHub.broadcast(Information(s"shown card ${card} from ${playerID}"))
      // TODO: send event
    } yield ()

  /** Draws a card from the draw deck, if no cards are available, piles are switched and shuffled If the card is a
    * exploding kitten it's discarded, otherwise it's added to the current player's hand
    * @return
    *   the card drawn
    */
  private def handleDrawCard(playerID: PlayerID, top: Boolean): IO[Card] =
    for {
      _ <- webSocketHub.broadcast(Information(s"$playerID is drawing a card"))
      card <- gameStateRef.modify { gameState =>
        val (deck, card) = if (top) gameState.drawDeck.draw else gameState.drawDeck.drawFromBottom
        (gameState.copy(drawDeck = deck), card)
      }
      currentPlayerMarkedBy <- gameStateRef.get.map(_.marking.find { case (_, from) => from == playerID }.map(_._1))
      playerToDraw <- currentPlayerMarkedBy.fold(playerID.pure[IO])(to =>
        webSocketHub.broadcast(Information(s"$playerID gave card to $to")) *> to.pure[IO]
      )

      hand <- gameStateRef.get.map(_.playersHands(playerToDraw)._1)

      _ <- card match {
        case ExplodingKitten if hand.count(_ == StreakingKitten) > hand.count(_ == ExplodingKitten) =>
          addCardToHand(playerToDraw, card)

        case ExplodingKitten =>
          for {
        _<- tryToDefuseCard(playerToDraw, card)
          } yield ()

        case BarkingKitten =>
          for {
            barking <- gameStateRef.get.map(_.barkingKitten)
            _ <- barking.fold(addCardToHand(playerToDraw, card))(player => tryToDefuseCard(player, card)

            )

          } yield ()
        case ImplodingKitten(true) =>
          for {
            _ <- webSocketHub.broadcast(Information(s"$playerID imploded"))
            _ <- killPlayer(playerToDraw, card)
          } yield ()
        case ImplodingKitten(false) =>
          for {
            _ <- webSocketHub.broadcast(Information(s"$playerID drew faced-down ${card.title}"))
            _ <- buryCard(playerID, ImplodingKitten(true))
          } yield ()

        case card => addCardToHand(playerToDraw, card)
      }
      hands <- getAllShownCardsInHands
      _     <- webSocketHub.broadcast(PlayersHands(hands.toList))
      _     <- gameStateRef.get.map(state => webSocketHub.broadcast(PilesUpdate(state.drawDeck.length)))

      _ <- webSocketHub.sendToPlayer2(playerID)(DrawCard(card, none))
    } yield card

  private def addCardToHand(player: PlayerID, card: Card): IO[Unit] =
    for {
      hand <- gameStateRef.modify(state => {

        val (hand, count) = state.playersHands(player)
        val newHand       = hand :+ card

        (
          state.copy(
            playersHands = state.playersHands + (player -> (newHand, count.copy(hidden = count.hidden + 1)))
          ),
          newHand
        )
      })
      _ <- webSocketHub.sendToPlayer2(player)(DrawCard(card, player.some))
      _ <- webSocketHub.sendToPlayer2(player)(CardsInHand(hand))

    } yield ()

  private def discard(card: Card): IO[Unit] =
    for {
      state <- gameStateRef.updateAndGet(state => state.copy(
        discardDeck = state.discardDeck.prepend(card)))

      _ <- webSocketHub.broadcast(PlayCard(card, none))

    } yield ()


  /** Removes all bombs and defuses from the deck and deals the players 7 cards + a defuse then adds the bombs back and
    * shuffles the deck
    */
  private def handCards(nPlayers: Int): IO[Unit] = {
    webSocketHub.broadcast(Information("Handing cards...")) *>
      gameStateRef
        .modify { gameState =>
          val (deckWOBombs, bombs) = removeDefuseAndBombs(gameState.drawDeck, recipe.defusesOnStart * nPlayers)
          val (deck, map) = gameState.players.foldLeft((deckWOBombs, Map.empty[PlayerID, (Hand, HandCount)])) {
            case ((cards, map), p) =>
              val (hand, newDrawDeck) = cards.splitAt(recipe.cardsOnStart(nPlayers))
              val newHand             = hand :+ Defuse

              (newDrawDeck, map + (p.playerID -> (newHand, HandCount(newHand.length, List()))))
          }

          val newDrawDeck = Deck(deck ++ bombs).shuffled
          (gameState.copy(drawDeck = newDrawDeck, playersHands = map), map)

        }
        .flatMap(map => map.toList.traverse_ { case (p, (h, _)) => webSocketHub.sendToPlayer2(p)(CardsInHand(h)) })

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
      val hand     = gameState.playersHands(playerID)._1
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

  private def playCard(index: Int, playerID: PlayerID): IO[Card] =
    for {
      card <- gameStateRef
        .modify { gameState =>
          val hand          = gameState.playersHands(playerID)
          val card          = hand._1(index)
          val (left, right) = hand._1.splitAt(index)
          val newCards      = left ::: right.drop(1)

          val hc =
            if (hand._2.shown.contains(card))
              hand._2.copy(shown = hand._2.shown.diff(List(card)))
            else hand._2.copy(hidden = hand._2.hidden - 1)

          (
            gameState.copy(
              discardDeck = gameState.discardDeck.prepend(card),
              playersHands = gameState.playersHands + (playerID -> (newCards, hc))
            ),
            card
          )
        }

      hands <- getAllShownCardsInHands
      _     <- webSocketHub.broadcast(PlayCard(card, playerID.some))
      _     <- webSocketHub.broadcast(PlayersHands(hands.toList))
    } yield card

  // -----------prompts cards-----------------//



  /** Prompt for Bury card. Asks player where they want to bury card and when valid, inserts the card at that index
    * @param playerID
    *   current player's id
    * @param card
    *   card to bury (hidden from the player)
    */
  private def buryCard(playerID: PlayerID, card: Card): IO[Unit] = {
    for {
      deckLength <- gameStateRef.get.map(_.drawDeck.length + 1)
      _          <- webSocketHub.sendToPlayer2(playerID)(BuryCard(card.some, 1, deckLength))
      string     <- webSocketHub.getGameInput(playerID).map(_.toIntOption)
      _ <- string match {
        case Some(index) if (0 until deckLength).contains(index - 1) =>
          updateDrawDeck(_.insertAt(index - 1, card))
        case _ =>
          webSocketHub.sendToPlayer2(playerID)(Error("Invalid input")) *> buryCard(playerID, card)
      }

    } yield ()

  }

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
      _ <- webSocketHub.sendToPlayer(playerID)("Who do you want to steal from?")
      playersFilter = players.filterNot(_.playerID == playerID)
      _ <- webSocketHub.sendToPlayer(playerID)(
        playersFilter.map(_.playerID).toString()
      )
      _      <- webSocketHub.sendToPlayer(playerID)("Insert index >> ")
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
              webSocketHub.sendToPlayer(playerID)("Invalid index") *> stealFromPlayer(
                playerID,
                players,
                isRandom
              )
          }
        case None =>
          webSocketHub.sendToPlayer(playerID)("Invalid input") *> stealFromPlayer(
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
    (if (cardOption.isDefined) webSocketHub.broadcast(s"$toID is trying to steal a ${cardOption.get} from $fromID")
     else webSocketHub.broadcast(s"$toID is stealing a card from $fromID")) *> {
      gameStateRef
        .modify { gameState =>
          val hands = gameState.playersHands

          (hands.get(toID), hands.get(fromID)) match {
            case (_, None) => (gameState, None)
            case (Some((to, toHC)), Some((from, fromHC))) =>
              val index = cardOption.fold({
                Random.nextInt(from.length)
              })(card => from.indexOf(card))

              from.get(index) match {
                case Some(card) =>
                  val newTo = to.appended(card)

                  val (a, b)  = from.splitAt(index)
                  val newFrom = a ++ b.drop(1)

                  val toHandCount = toHC.copy(hidden = toHC.hidden + 1)

                  val fromHandCount =
                    if (fromHC.shown.contains(card))
                      fromHC.copy(shown = fromHC.shown.diff(List(card)))
                    else fromHC.copy(hidden = fromHC.hidden - 1)

                  val newHands = hands + (fromID -> (newFrom, fromHandCount)) + (toID -> (newTo, toHandCount))

                  (gameState.copy(playersHands = newHands), Some(card))

                case None => (gameState, None)
              }

          }

        }
        .flatMap { optCard =>
          optCard.fold(webSocketHub.broadcast(s"$toID couldn't steal card from $fromID"))(card =>
            webSocketHub.broadcast(s"$toID stole a card from $fromID") *> webSocketHub.sendToPlayer(toID)(
              s"Stole $card from $fromID"
            ) *> webSocketHub.sendToPlayer(fromID)(s"$toID stole your $card")
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
      _      <- webSocketHub.sendToPlayer(toID)("What card do you want?")
      _      <- webSocketHub.sendToPlayer(toID)(cardsPossible.toString())
      _      <- webSocketHub.sendToPlayer(toID)("Insert index >> ")
      string <- webSocketHub.getGameInput(toID).map(_.toIntOption)
      card <- string match {
        case Some(index) =>
          index match {
            case i if (1 to cardsPossible.length) contains i =>
              cardsPossible(i - 1).pure[IO]
            case _ =>
              webSocketHub.sendToPlayer(toID)("Invalid choice") *> pickFromPlayer(fromID, toID)
          }
        case None =>
          webSocketHub.sendToPlayer(toID)("Invalid input") *> pickFromPlayer(fromID, toID)
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
        .map { state =>
          state.playersHands.map { case (id, (hand, _)) => id -> hand }
        }
        .map(_ - playerID)
        .map(_.filter {
          case (_, hand) if hand.contains(Nope) => true
          case _                                => false
        })
      result <-
        IO.race(
          if (playerHandsWithNope.isEmpty) IO.unit
          else
            webSocketHub.broadcast("Checking if action is noped") *> IO.sleep(
              100.millis
            ) *> broadCastCountDown(5) *> false.pure[IO],
          for {
            _ <- playerHandsWithNope.keys.toList.parTraverse { pID =>
              webSocketHub.sendToPlayer(pID)(s"$pID, do you wish to nope this action? (y/n)")
            }
            ans <- webSocketHub.getPendingInputs(playerHandsWithNope.keys.toList, message = "y")
            res <- ans.fold(false.pure[IO]) { pID =>
              playCard(playerHandsWithNope(pID).indexOf(Nope), pID) *> webSocketHub.broadcast(
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
        _ <- webSocketHub.broadcast(s"$counter")
        _ <- IO.sleep(1.seconds)
        _ <- broadCastCountDown(counter - 1)
      } yield ()

}

object Game {

  /** Creates a new game for a number of players and a websockethub
    * @return
    *   game created
    */
  def create(
      roomName: String,
      recipe: Recipe
  ): IO[Game] = {

    val currentPlayerIndex = -1
    val playersList        = List.empty

    val initialState = State(
      started = false,
      Deck(List.empty),
      Deck(List.empty),
      currentPlayerIndex,
      1,
      playersList,
      Map.empty,
      Map.empty,
      List.empty,
      orderRight = true,
      skippedPlayingCard = false,
      none
    )

    for {
      webSocketHub <- WebSocketHub.of
//      stateManager <- StateManager.of(initialState, webSocketHub)
      gameStateRef <- Ref.of[IO, State](initialState)
      prompter = PromptsHandler(webSocketHub)
      deferred <- Deferred[IO, Boolean]
    } yield new Game(roomName, webSocketHub, gameStateRef, /*stateManager,*/ prompter, recipe, started = deferred)
  }
}
