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

import scala.concurrent.duration.{DurationDouble, DurationInt}
import scala.util.Random

case class Game(
    roomName: String,
    webSocketHub: WebSocketHub,
    gameStateRef: Ref[IO, State],
    prompter: PromptsHandler,
    recipe: Recipe,
    started: Deferred[IO, Boolean]
) {

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

  /** initializes the game: initializes the deck, hands cards and selects a starting player
    */
  private def initialize(): IO[Unit] = {
    gameStateRef.get.flatMap { gameState =>
      for {
        _ <- webSocketHub.broadcast(Started())
        nPlayers = gameState.players.length
        drawDeck = initFromRecipe(recipe, gameState.players.length)
        _      <- gameStateRef.update(_.copy(drawDeck = drawDeck))
        _      <- handCards(nPlayers)
        _      <- gameStateRef.get.flatMap(state => webSocketHub.broadcast(PilesUpdate(state.drawDeck.length)))
        player <- setRandomStartingPlayer()
        _      <- gameLoop(player).start
      } yield ()
    }
  }

  /** Resets the game state
    */
  def reset(): IO[Unit] =
    gameStateRef.update(_ =>
      State(
        started = false,
        Deck(List.empty),
        Deck(List.empty),
        -1,
        1,
        List.empty,
        Map.empty,
        Map.empty,
        List.empty,
        orderRight = true,
        none,
        skipped = false,
        none
      )
    )

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
            _ <- webSocketHub.sendToPlayer(playerID)(RoomStateEvent(state.players, recipe))
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
            _ <- webSocketHub.sendToPlayer(playerID)(RoomStateEvent(newState.players, recipe))
          } yield Right()
        }
    } yield res

  /** Reconnects a player
    * @param playerID
    *   \- player that reconnected
    * @return
    */
  private def reconnect(playerID: PlayerID): IO[Unit] = {
    for {
      handOpt <- getHand(playerID)
      _ <- handOpt.fold(IO.println("recon failed to update hand"))(hand =>
        webSocketHub.sendToPlayer(playerID)(CardsInHand(hand)) // should re-send events they missed
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
  private def setRandomStartingPlayer(): IO[PlayerID] =
    webSocketHub.broadcast(Information("Selecting a random player to start...")) *> gameStateRef
      .modify { gameState =>
        val index = Random.nextInt(gameState.players.length)
        (gameState.copy(currentPlayerIndex = index, turnsLeft = 1), gameState.players(index).playerID)
      }
      .flatTap(player => webSocketHub.broadcast(Information(s"$player's starting")))

  /** Sets the current player index to the next player
    */
  private def rightOfPlayer(): IO[Unit] =
    gameStateRef.update { gameState =>
      val index = gameState.currentPlayerIndex + 1 match {
        case x if (1 until gameState.players.length).contains(x) => x
        case _                                                   => 0
      }
      gameState.copy(currentPlayerIndex = index)
    }

  /** Sets the current player index to the previous player
    */
  private def leftOfPlayer(): IO[Unit] =
    gameStateRef.update { gameState =>
      val index = gameState.currentPlayerIndex - 1 match {
        case x if x < 0 => gameState.players.length - 1
        case x          => x
      }

      gameState.copy(currentPlayerIndex = index)
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

  private def nextPlayer(): IO[Unit] =
    for {
      nextPlayerI <- gameStateRef.get.map(_.nextPlayerIndex)
      turnsLeft   <- gameStateRef.get.map(_.turnsLeft)
      _ <- nextPlayerI.fold(
        if (turnsLeft > 1) IO.unit // same player
        else
          gameStateRef.get.flatMap(gameState => if (gameState.orderRight) rightOfPlayer() else leftOfPlayer())
      )(index => gameStateRef.update(_.copy(currentPlayerIndex = index)))
      _ <- gameStateRef.update(state =>
        state.copy(
          turnsLeft = state.turnsLeft match {
            case 1 => 1
            case x => x - 1
          },
          skipped = false,
          nextPlayerIndex = none
        )
      )
    } yield ()

  private def getCurrentPlayer: IO[PlayerID] =
    gameStateRef.get.map { gameState =>
      val index = gameState.currentPlayerIndex
      gameState.players(index).playerID
    }

  private def getNextPlayer: IO[PlayerID] = {
    gameStateRef.get.flatMap(gameState => if (gameState.orderRight) getRightOfPlayer else getLeftOfPlayer)
  }

  private def previousPlayer(): IO[Unit] = {
    gameStateRef.get.flatMap(gameState => if (gameState.orderRight) leftOfPlayer() else rightOfPlayer())
  }

  private def reverseOrder(): IO[Unit] = {
    gameStateRef.update(gameState => gameState.copy(orderRight = !gameState.orderRight))
  }

  /** Sets the next player index to the index of a given player
    * @param playerID
    *   player to set as the next one
    */
  private def setNextPlayer(playerID: PlayerID): IO[Unit] =
    gameStateRef.update { gameState =>
      val index = gameState.players.map(_.playerID).indexOf(playerID)
      gameState.copy(nextPlayerIndex = index.some)
    }

  /** Adds amount of turns to turns Left
    * @param turns
    *   how many
    */
  private def addTurns(turns: Int): IO[Unit] =
    gameStateRef.update { state =>
      state.copy(turnsLeft = turns + state.turnsLeft)
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
      .flatMap(_ => webSocketHub.broadcast(Died(playerID)))

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
      case None => nextPlayer() *> getCurrentPlayer.flatMap(np => gameLoop(np))
    })
  }

  private def debugTurn(): IO[Unit] = for {
    state <- gameStateRef.get
    _     <- IO.println(s"discard: ${state.discardDeck}")
    _     <- IO.println(s"draw: ${state.drawDeck}")
    _     <- IO.println(s"${state.players(state.currentPlayerIndex)} for ${state.turnsLeft}")
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
        _    <- debugTurn()
        _    <- webSocketHub.broadcast(NewTurn(playerID))
        _    <- playCardsLoop(playerID)
        skip <- gameStateRef.get.map(_.skipped)
        _ <-
          if (!skip) {
            for {
              _         <- handleDrawCard(playerID, top = true, reveal = true)
              gameState <- gameStateRef.get
              _ <- webSocketHub.broadcast(
                PilesUpdate(gameState.drawDeck.length)
              )

            } yield ()
          } else IO.unit
        handOpt <- getHand(playerID)
        _ <- handOpt.fold(IO.unit)(hand => webSocketHub.sendToPlayer(playerID)(CardsInHand(hand)))
      } yield ()
    )
    .void

  private def playCardsLoop(playerID: PlayerID): IO[Unit] =
    (for {
      hand <- getHand(playerID)
      skip <- gameStateRef.get.map(_.skipped)
      pass <- hand
        .fold(true.pure[IO])(hand =>
          if (skip) true.pure[IO]
          else
            playOneCard(playerID, hand)
        )
    } yield pass).flatMap(pass =>
      if (pass) {
        IO.unit
      } else {
        playCardsLoop(playerID)
      }
    )


//return if player wants to play more
  private def playOneCard(playerID: PlayerID, hand: Hand): IO[Boolean] =
    for {
      playedCards <- prompter.playCardsPrompt(playerID, hand)
      res <- playedCards.fold(true.pure[IO])(list =>
        (  for {
          cardsPlayed <- playCards(list, playerID)
          _       <- handleCardPlayed(playerID, cardsPlayed)
          handNew <- getHand(playerID)
          _       <- handNew.fold(IO.unit)(hand => webSocketHub.sendToPlayer(playerID)(CardsInHand(hand)))

        } yield ()).flatMap(_ => false.pure[IO])
      )

    } yield res

  private def getHand(playerID: PlayerID): IO[Option[Hand]] =
    gameStateRef.get.map { state =>
      state.playersHands.find { case (pID, _) => pID == playerID }.map(_._2).map(_._1)
    }

  private def getAllShownCardsInHands: IO[Map[PlayerID, HandCount]] =
    gameStateRef.get.map { state =>
      state.playersHands.map { case (id, (_, count)) => (id, count) }
    }

  private def tryToDefuseCard(playerID: PlayerID, card: Card): IO[Unit] =
    for {
      _ <- webSocketHub.broadcast(Information(s"$playerID drew ${card.title}"))
      _ <- IO.sleep(2.seconds)
      defuseOpt <- gameStateRef.get.map { gameState =>
        val hand     = gameState.playersHands(playerID)._1
        hand.zipWithIndex
          .collectFirst { case (Defuse, i) => i }
      }
      hand <- gameStateRef.get.map { gameState =>
        gameState.playersHands(playerID)._1
      }
      _ <- defuseOpt.fold(webSocketHub.broadcast(PlayCard(card,playerID.some)) *> webSocketHub.sendToPlayer(playerID)(CardsInHand(hand)) *> IO.sleep(1.seconds) *> killPlayer(playerID, card))(index =>
        for {
          _ <- playCards(index :: Nil, playerID)
          _ <- IO.sleep(2.seconds)
          _ <- buryCard(playerID, card, reveal = true)
          newHand <- getHand(playerID)
          _ <- webSocketHub.sendToPlayer(playerID)(CardsInHand(newHand.get))
        } yield ()
      )
    } yield ()

  /** Handles the card played, performing the necessary operations
    * @param player
    *   current player
    * @param cards
    *   cards played
    * @return
    *   if the played card caused the player to skip drawing a card
    */
  private def handleCardPlayed(player: PlayerID, cards: List[Card]): IO[Unit] =
    for {
      playerHands <- gameStateRef.get
        .map { state =>
          state.playersHands.map { case (id, (hand, _)) => id -> hand }
        }
        .map(_ - player)
      res <- if (cards.contains(BarkingKitten)) false.pure[IO] else getNopes(playerHands)
      _ <- cards match {
        case ls if ls.length == 3 =>
          for {
            players <- gameStateRef.get.map(_.players.map(_.playerID).filterNot(_ == player))
            _ <- stealFromPlayer(player, players, isRandom = false)
          } yield ()

        case ls if ls.length == 2 =>
          for {
            players <- gameStateRef.get.map(_.players.map(_.playerID).filterNot(_ == player))
            _ <- stealFromPlayer(player, players, isRandom = true)
          } yield ()
        case card::Nil =>
          if (res) IO.unit
          else
            for {
              _ <- card match {
                case SuperSkip => gameStateRef.update(state => state.copy(turnsLeft = 1, skipped = true))
                case _: Skipper => gameStateRef.update(state => state.copy(skipped = true))
                case _ => IO.unit
              }
              _ <- card match {
                case Shuffle =>
                  updateDrawDeck(_.shuffled)

                case AlterTheFuture3X =>
                  for {
                    cards3 <- gameStateRef.get.map(_.drawDeck.getFirstN(3))
                    order <- prompter.alterTheFuture(cards3, player)
                    _ <- updateDrawDeck(_.alterTheFuture3X(order))
                  } yield ()

                case AlterTheFuture3XNOW =>
                  for {
                    cards3 <- gameStateRef.get.map(_.drawDeck.getFirstN(3))
                    order <- prompter.alterTheFuture(cards3, player)
                    _ <- updateDrawDeck(_.alterTheFuture3X(order))
                  } yield ()

                case SwapTopAndBottom =>
                  updateDrawDeck(_.swapTopAndBottom)

                case Attack2X =>
                  for {
                    np <- getNextPlayer
                    _ <- setNextPlayer(np)
                    _ <- addTurns(2)
                  } yield ()

                case TargetedAttack2X =>
                  for {
                    players <- gameStateRef.get.map(_.players.filterNot(_.playerID == player).map(_.playerID))
                    nextPlayer <- prompter.choosePlayer(player, players)
                    _ <- setNextPlayer(nextPlayer)
                    _ <- addTurns(2)
                  } yield ()

                case CatomicBomb =>
                  updateDrawDeck(_.withExplodingKittensOnTop)

                case Bury =>
                  for {
                    card <- handleDrawCard(player, top = true, reveal = false)
                    _ <- buryCard(player, card, reveal = false)
                    _ <- gameStateRef.update(state => state.copy(skipped = true))
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
                      case None => gameStateRef.update(state => state.copy(barkingKitten = player.some))
                    }
                  } yield ()
                case DrawFromTheBottom =>
                  handleDrawCard(player, top = false, reveal = true) *> gameStateRef.update(state => state.copy(skipped = true))
                case GarbageCollection => garbageCollection()
                case IllTakeThat =>
                  for {
                    players <- gameStateRef.get.map(_.players.filterNot(_.playerID == player))
                    chosenPlayer <- prompter.choosePlayer(player, players.map(_.playerID))
                    _ <- gameStateRef.update(state => state.copy(marking = state.marking :+ (player, chosenPlayer)))
                    _ <- webSocketHub.broadcast(Information(s"$chosenPlayer's next card will be taken by $player'"))
                  } yield ()
                case Mark =>
                  for {
                    players <- gameStateRef.get.map(_.players.filterNot(_.playerID == player))
                    chosenPlayer <- prompter.choosePlayer(player, players.map(_.playerID))
                    opt <- getHand(chosenPlayer)
                    _ <- opt.fold(IO.unit)(hand => showCardFromPlayer(chosenPlayer, getRandomFromList(hand)))
                  } yield ()
                case PersonalAttack3X => addTurns(2)
                case SeeTheFuture3X =>
                  gameStateRef.get
                    .map(_.drawDeck)
                    .flatMap(drawDeck =>
                      webSocketHub.sendToPlayer(player)(Information(s"Next 3 cards are ${drawDeck.getFirstN(3)}"))
                    )
                case SeeTheFuture5X =>
                  gameStateRef.get
                    .map(_.drawDeck)
                    .flatMap(drawDeck =>
                      webSocketHub.sendToPlayer(player)(Information(s"Next 5 cards are ${drawDeck.getFirstN(5)}"))
                    )
                case ShareTheFuture3X =>
                  for {
                    cards3 <- gameStateRef.get.map(_.drawDeck.getFirstN(3))
                    order <- prompter.alterTheFuture(cards3, player)
                    _ <- updateDrawDeck(_.alterTheFuture3X(order))
                    newCards <- gameStateRef.get.map(_.drawDeck.getFirstN(3))
                    nextPlayer <- getNextPlayer
                    _ <- webSocketHub.sendToPlayer(nextPlayer)(SeeCards(newCards))
                  } yield ()

                case _ => IO.unit
              }

            } yield ()

        case Nil => IO.unit
      }

    } yield ()

  private def garbageCollection(): IO[Unit] =
    for {
      playersWithCards <- gameStateRef.get.map(
        _.playersHands.filter { case (_, h) => h.nonEmpty }.map(a => a._1 -> a._2._1)
      )
      list  <- prompter.garbageCollectionPrompt(playersWithCards)
      cards <- list.traverse { case (player, index) => removeCardAt(player, index) }
      _ <- cards.traverse_(card => gameStateRef.update(state => state.copy(drawDeck = state.drawDeck.prepend(card))))
      _ <- gameStateRef.update(state => state.copy(drawDeck = state.drawDeck.shuffled))
      hands <- playersWithCards.toList.map({case (pid, _) =>
        getHand(pid).map(hand => hand.fold(IO.unit)(h =>
          webSocketHub.sendToPlayer(pid)(CardsInHand(h))))}).sequence
      _ <- hands.traverse(identity)
      piles <- gameStateRef.get.map(_.drawDeck.length)
      _ <- webSocketHub.broadcast(PilesUpdate(piles))
    } yield ()



  private def removeCardAt(player: PlayerID, index: Int): IO[Card] =
    gameStateRef.modify(state => {
      val hand          = state.playersHands(player)
      val card          = hand._1(index)
      val (left, right) = hand._1.splitAt(index)
      val newCards      = left ::: right.drop(1)

      val hc =
        if (hand._2.shown.contains(card))
          hand._2.copy(shown = hand._2.shown.diff(List(card)))
        else hand._2.copy(hidden = hand._2.hidden - 1)

      (
        state.copy(
          playersHands = state.playersHands + (player -> (newCards, hc))
        ),
        card
      )
    })

  // ___________ Deck Operations ______________________________//

  private def showCardFromPlayer(playerID: PlayerID, card: Card): IO[Unit] =
    for {
      _ <- gameStateRef.update(state => {
        val (hand, HandCount(hidden, shown)) = state.playersHands(playerID)
        state.copy(playersHands = state.playersHands + (playerID -> (hand, HandCount(hidden - 1, shown :+ card))))
      })
      _ <- webSocketHub.broadcast(Information(s"shown card $card from $playerID"))
      hands <- getAllShownCardsInHands
      _ <- webSocketHub.broadcast(PlayersHands(hands.toList))
    } yield ()

  /** Draws a card from the draw deck, if no cards are available, piles are switched and shuffled If the card is a
    * exploding kitten it's discarded, otherwise it's added to the current player's hand
    * @return
    *   the card drawn
    */
  private def handleDrawCard(playerID: PlayerID, top: Boolean, reveal: Boolean): IO[Card] =

    for {
      _ <- webSocketHub.broadcast(Information(s"$playerID is drawing a card"))
      card <- gameStateRef.modify { gameState =>
        val (deck, card) = if (top) gameState.drawDeck.draw else gameState.drawDeck.drawFromBottom
        (gameState.copy(drawDeck = deck), card)
      }
      currentPlayerMarkedBy <- gameStateRef.modify(state => {
        val result = state.marking.zipWithIndex.find { case ((_, from), _) => from == playerID }

        val markings = result.fold(state.marking)({case ((_,_), i) => state.marking.zipWithIndex.filter { case ((_,_),`i`) => false}.map(_._1)})


        (state.copy(marking = markings), result.map(_._1).map(_._1))
      })
      playerToDraw <- currentPlayerMarkedBy.fold(playerID.pure[IO])(to =>
        webSocketHub.broadcast(Information(s"$playerID gave card to $to")) *> to.pure[IO]
      )

      hand <- gameStateRef.get.map(_.playersHands(playerToDraw)._1)
      _ <- if(reveal) webSocketHub.sendToPlayer(playerToDraw)(DrawCard(card, playerToDraw.some)) else IO.unit
      _ <- IO.sleep(1.5.seconds)
      _ <- if(reveal) webSocketHub.sendToPlayer(playerToDraw)(CardsInHand(hand :+ card))else IO.unit
      _ <- card match {
        case ExplodingKitten if hand.count(_ == StreakingKitten) > hand.count(_ == ExplodingKitten) =>
          addCardToHand(playerToDraw, card)

        case ExplodingKitten =>
          for {
            _ <- tryToDefuseCard(playerToDraw, card)
          } yield ()

        case BarkingKitten =>
          for {
            barking <- gameStateRef.get.map(_.barkingKitten)
            _       <- barking.fold(addCardToHand(playerToDraw, card))(player => tryToDefuseCard(player, card))

          } yield ()
        case ImplodingKitten(true) =>
          for {
            _ <- webSocketHub.broadcast(Information(s"$playerID imploded"))
            _ <- killPlayer(playerToDraw, card)
          } yield ()
        case ImplodingKitten(false) =>
          for {
            _ <- webSocketHub.broadcast(Information(s"$playerID drew a faced-down ${card.title}"))
            _ <- IO.sleep(1.seconds)
            _ <- buryCard(playerID, ImplodingKitten(true), reveal = true)
          } yield ()

        case card => addCardToHand(playerToDraw, card)
      }
      hands <- getAllShownCardsInHands
      _     <- webSocketHub.broadcast(PlayersHands(hands.toList))
      _     <- gameStateRef.get.map(state => webSocketHub.broadcast(PilesUpdate(state.drawDeck.length)))

    } yield card

  private def addCardToHand(player: PlayerID, card: Card): IO[Unit] =
    for {
      _ <- gameStateRef.modify(state => {

        val (hand, count) = state.playersHands(player)
        val newHand       = hand :+ card

        (
          state.copy(
            playersHands = state.playersHands + (player -> (newHand, count.copy(hidden = count.hidden + 1)))
          ),
          newHand
        )
      })

    } yield ()

  // understand barking kitten
  private def discard(card: Card): IO[Unit] =
    for {
      _ <- gameStateRef.updateAndGet(state => state.copy(discardDeck = state.discardDeck.prepend(card)))

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
        .flatTap(_ => getAllShownCardsInHands.flatMap(hands => webSocketHub.broadcast(PlayersHands(hands.toList))))
        .flatMap(map => map.toList.traverse_ { case (p, (h, _)) => webSocketHub.sendToPlayer(p)(CardsInHand(h)) })

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

  /** Plays the card at given index, removing it from the player's hand, discarding it and returning the card
    *
    * @param indices
    *   the indices of the cards to play
    * @return
    *   Card played
    */

  private def playCards(indices: List[Int], playerID: PlayerID): IO[List[Card]] =
    for {
      cards <- gameStateRef
        .modify { gameState => {
          val hand = gameState.playersHands(playerID)
          val (playedCards, newHand) = hand._1.zipWithIndex.partition { case (_, index) => indices.contains(index) }

          val newCards = newHand.map(_._1)
          val cardsPlayed = playedCards.map(_._1)

          val newShown = hand._2.shown diff playedCards
          val newHidden = cardsPlayed.length - (hand._2.shown.length - newShown.length)


          (
            gameState.copy(
              discardDeck = gameState.discardDeck.concat(cardsPlayed),
              playersHands = gameState.playersHands + (playerID -> (newCards, HandCount(newHidden, newShown)))
            ),
            cardsPlayed
          )
        }
        }

      hands <- getAllShownCardsInHands
      _     <- cards.traverse_(card => webSocketHub.broadcast(PlayCard(card, playerID.some)))
      handOpt <- getHand(playerID)
      _ <- handOpt.fold(IO.unit)(hand => webSocketHub.sendToPlayer(playerID)(CardsInHand(hand)))
      _     <- webSocketHub.broadcast(PlayersHands(hands.toList))
    } yield cards

  // -----------prompts cards-----------------//

  /** Prompt for Bury card. Asks player where they want to bury card and when valid, inserts the card at that index
    * @param playerID
    *   current player's id
    * @param card
    *   card to bury (hidden from the player)
    */
  private def buryCard(playerID: PlayerID, card: Card, reveal: Boolean): IO[Unit] = {
    for {
      deckLength <- gameStateRef.get.map(_.drawDeck.length + 1)
      _          <- webSocketHub.sendToPlayer(playerID)(BuryCard(if(reveal) card.some else None, 1, deckLength))
      string     <- webSocketHub.getGameInput(playerID).map(_.toIntOption)
      _ <- string match {
        case Some(index) if (0 until deckLength).contains(index - 1) =>
          updateDrawDeck(_.insertAt(index - 1, card))
        case _ =>
          webSocketHub.sendToPlayer(playerID)(Error("Invalid input")) *> buryCard(playerID, card, reveal)
      }

    } yield ()

  }

  /** Prompt for choosing a player to steal a card from, then steals it either randomly or asks the player to name a
    * card
    * @param player
    *   current player's id
    * @param players
    *   list of current players
    * @param isRandom
    *   if the card stolen should be random
    */
  private def stealFromPlayer(player: PlayerID, players: List[PlayerID], isRandom: Boolean): IO[Unit] =
    for {
      chosenPlayer <- prompter.choosePlayer(player, players)

      card <-
        if (!isRandom)
          prompter.chooseCard(
            player,
            recipe.getCardMap
              .map(_._1)
              .filter({
                case ExplodingKitten    => false
                case ImplodingKitten(_) => false
                case _                  => true
              })
          )
        else
          for {
            chosenHand <- gameStateRef.get.map(_.playersHands(chosenPlayer)._1)
            random = Random.nextInt(chosenHand.length)
          } yield chosenHand(random)

      _     <- stealCard(chosenPlayer, player, card)
      hands <- getAllShownCardsInHands
      _     <- webSocketHub.broadcast(PlayersHands(hands.toList))
      _ <-getHand(player).flatMap(hand =>hand.fold(IO.unit)(h => webSocketHub.sendToPlayer(player)(CardsInHand(h))))
      _ <-getHand(chosenPlayer).flatMap(hand =>hand.fold(IO.unit)(h => webSocketHub.sendToPlayer(chosenPlayer)(CardsInHand(h))))
    } yield ()

  /** Tries to steal a card from a given player
    * @param fromID
    *   id of the player to steal from
    * @param toID
    *   if of the player to receive the card
    */
  private def stealCard(fromID: PlayerID, toID: PlayerID, cardPicked: Card): IO[Unit] =
    gameStateRef
      .modify { gameState =>
        val hands = gameState.playersHands

        (hands.get(toID), hands.get(fromID)) match {
          case (_, None) => (gameState, None)
          case (Some((to, toHC)), Some((from, fromHC))) =>
            val cardOption = from.find(_ == cardPicked)

            cardOption match {
              case Some(card) =>
                val newTo = to.appended(card)

                val newFrom = from diff List(card)

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
        optCard.fold(webSocketHub.broadcast(Information(s"$toID couldn't steal card from $fromID")))(card =>
          webSocketHub.broadcast(Information(s"$toID stole a card from $fromID")) *> webSocketHub.sendToPlayer(toID)(
            Information(s"Stole $card from $fromID")
          ) *> webSocketHub.sendToPlayer(fromID)(Information(s"$toID stole your $card"))
        )
      }

  /** ask players if they want to nope this action, returns true on the first player that answers yes or false if runs
    * out of time
    * @param playerHands
    *   players hands
    * @return
    *   true if anyone noped
    */
  private def getNopes(
      playerHands: Map[PlayerID, Hand]
  ): IO[Boolean] = {
    for {
      deferred <- Deferred[IO, (PlayerID, Int)]

      res <- IO
        .race(
          IO.sleep(
            100.millis
          ) *> webSocketHub.broadcast(Timer(2)) *> prompter.broadCastCountDown(2) *> false.pure[IO],
          for {
            _ <- playerHands.toList.parTraverse { case (pID, hand) =>
              getNopeFrom(pID, hand, deferred)
            }
            res <- deferred.get
            (player, index) = res
            res <- playCards(index::Nil, player) *> true.pure[IO]
          } yield res
        )
        .flatTap(_ => webSocketHub.broadcast(EndNopes()))
        .flatMap {
          case Left(_)      => false.pure[IO]
          case Right(value) => value.pure[IO]
        }
      _ <- IO.println(s"res is $res")
    } yield res
  }
  private def getNopeFrom(playerID: PlayerID, hand: Hand, deferred: Deferred[IO, (PlayerID, Int)]): IO[Unit] =
    for {
      _    <- webSocketHub.sendToPlayer(playerID)(GetNopes(Nil))
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

    val initialState = State(
      started = false,
      Deck(List.empty),
      Deck(List.empty),
      -1,
      1,
      List.empty,
      Map.empty,
      Map.empty,
      List.empty,
      orderRight = true,
      none,
      skipped = false,
      none
    )

    for {
      webSocketHub <- WebSocketHub.of
      gameStateRef <- Ref.of[IO, State](initialState)
      prompter = PromptsHandler(webSocketHub)
      deferred <- Deferred[IO, Boolean]
    } yield new Game(roomName, webSocketHub, gameStateRef, prompter, recipe, started = deferred)
  }
}
