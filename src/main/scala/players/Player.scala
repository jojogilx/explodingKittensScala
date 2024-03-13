package players

import card._
import cats.effect.IO
import cats.effect.std.Queue
import cats.implicits.catsSyntaxOptionId
import players.Player._

object Player {
  type PlayerID = String
  private type Hand = List[Card]

  //see if smt should be here
}

case class Player(playerID: PlayerID, receiveQueue: Queue[IO, String], color: String) {
  private var cards = List.empty[Card] //todo: remove var

  /**
   * Returns the list of cards the player is holding
   * @return Hand
   */
  def hand: Hand = {
    val ret = cards
    ret
  }

  /**
   * Tries to find a defuse in the cards the player is holding
   * @return Option of defuse card
   */
  def tryGetDefuse: Option[Card] =
    cards.zipWithIndex.find { case (card, _) => card match {
      case Defuse => true
      case _ => false
    }} match {
      case Some((card, i)) =>
        val (left, right) = cards.splitAt(i)
        cards = left ::: right.drop(1)
        card.some
      case None => None
    }

  /**
   * Plays the card at given index, removing it from the player's hand and returning the card
   * @param index - the index of the card to play
   * @return Card played
   */
  def playCard(index: Int): Card = {
    val card = cards(index)
    val (left, right) = cards.splitAt(index)
    cards = left ::: right.drop(1)
    card
  }

  /**
   * Initializes the first hand of the game
   * @param hand - initial list of cards
   */
  def initHand(hand: Hand): Unit = {
    cards = hand
  }

  /**
   * Adds a card to the players' hand
   * @param card - card to add
   */
  def drawCard(card: Card): Unit = {
    cards = cards :+ card
  }

  /**
   * returns a string representation of the players' hand, 1-indexed
   * @return string representation of the hand
   */
  def handWithIndex(): String = {
    cards.zipWithIndex.map {
      case (card, i) => s"${i + 1}. $card   "
    }.mkString
  }

  def sendMessage(message: String): Unit = {
    for {
     _ <- receiveQueue.offer(message)
    } yield ()
  }

}

