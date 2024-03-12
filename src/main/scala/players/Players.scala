package players

import card.Cards.{Card, Defuse, Hand}
import cats.implicits.catsSyntaxOptionId
import utils.Utils.TerminalUtils.Reset

import scala.util.control.Breaks

object Players {

  type PlayerID = String
  private type Hand = List[Card]

  case class Player(playerID: PlayerID, textColor: String) {
    private var cards = List.empty[Card]

    def hand: Hand = {
      val ret = cards
      ret
    }

    def tryGetDefuse: Option[Card] =
      cards.zipWithIndex.find { case (card, _) => card match {
        case Defuse() => true
        case _ => false
      }} match {
        case Some((card, i)) =>
          val (left, right) = cards.splitAt(i)
          cards = left ::: right.drop(1)
          card.some
        case None => None
      }

    def playCard(index: Int): Card = {
      val card = cards(index)
      val (left, right) = cards.splitAt(index)
      cards = left ::: right.drop(1)
      card
    }

    def initHand(hand: Hand): Unit = {
      cards = hand
    }

    def drawCard(card: Card): Unit = {
      cards = cards :+ card
    }


    def handWithIndex(): String = {
      cards.zipWithIndex.map {
        case (card, i) => s"${i + 1}. $card   "
      }.mkString
    }


  }

}
