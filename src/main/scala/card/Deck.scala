package card

import card.Cards._
import utils.Utils.intWithTimes

import scala.::
import scala.language.reflectiveCalls
import scala.util.Random

object Deck {
  def initShuffledNopeSauce(nPlayers: Int): Deck = {

    println("dealing nope sauce...")
    val cards : List[Card] = {

      (1 to 3).foldLeft(List.empty[Card]) { (acc, _) =>
        Bury() :: acc
      } ++
      (1 to 4).foldLeft(List.empty[Card]) { (acc, _) =>
        Tacocat() :: acc
      } ++
      (1 to 6).foldLeft(List.empty[Card]) { (acc, _) =>
        FeralCat() :: acc
      } ++
      (1 to 4).foldLeft(List.empty[Card]) { (acc, _) =>
        Reverse() :: acc
      } ++ (1 until nPlayers).foldLeft(List.empty[Card]) { (acc, _) =>
        ExplodingKitten() :: acc
      } ++
      (1 to 4).foldLeft(List.empty[Card]) { (acc, _) =>
        Shuffle() :: acc
      } ++
      (1 to 4).foldLeft(List.empty[Card]) { (acc, _) =>
        Skip() :: acc
      } ++
      (1 to 2).foldLeft(List.empty[Card]) { (acc, _) =>
        AlterTheFuture3X() :: acc
      } ++
      (1 to 2).foldLeft(List.empty[Card]) { (acc, _) =>
        SwapTopAndBottom() :: acc
      } ++
      (1 to 3).foldLeft(List.empty[Card]) { (acc, _) =>
        Attack2X() :: acc
      } ++
      (1 to 2).foldLeft(List.empty[Card]) { (acc, _) =>
        TargetedAttack2X() :: acc
      } ++
      (1 to 8).foldLeft(List.empty[Card]) { (acc, _) =>
        Nope() :: acc
      }

    } :+ CatomicBomb ()

    Deck(cards).shuffled
  }

  def initFromDiscardPile(discardPile: Deck): Deck = {
    Deck(discardPile.cards).shuffled
  }


}
  case class Deck(private val cards: List[Card]) {

    def shuffled: Deck = Deck(Random.shuffle(cards))
    def reversed: Deck = Deck(cards.reverse)

    def draw: Option[(Deck, Card)] = cards match {
      case Nil => None
      case card :: newDeck => Some(Deck(newDeck), card)
    }

    def insertAt(index: Int, card: Card): Deck = {
      val (before, after) = cards.splitAt(index)
      Deck(before ++ List(card) ++ after)
    }

    def prepend(card: Card): Deck = {
      Deck(card :: cards)
    }

    def swapTopAndBottom: Deck = {
      val first +: cards :+ last = this.cards
      Deck(last +: cards :+ first)
    }

    def removeBombs(deck: Deck): (List[Card], List[Card]) = {
      val cards = deck.cards.filter({ case ExplodingKitten() => false
       case _ => true
      })
      (cards,
        (1 to deck.cards.count({ case ExplodingKitten() => true
          case _ => false
        }))
          .foldLeft(List.empty[Card])  { (acc, _) =>
        ExplodingKitten() :: acc
      })
    }






}
