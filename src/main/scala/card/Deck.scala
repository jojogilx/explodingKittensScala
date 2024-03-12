package card

import scala.language.reflectiveCalls
import scala.util.Random

object Deck {

  /** Card types' count for nope sauce recipe
    */
  private val nopeSauceMap: Int => Map[Card, Int] = nPlayers =>
    Map(
      ExplodingKitten  -> (nPlayers - 1),
      Defuse           -> nPlayers,
      Nope             -> 8,
      Shuffle          -> 4,
      Skip             -> 4,
      AlterTheFuture3X -> 2,
      SwapTopAndBottom -> 2,
      Attack2X         -> 3,
      TargetedAttack2X -> 2,
      CatomicBomb      -> 1,
      Bury             -> 3,
      Tacocat          -> 4,
      FeralCat         -> 6,
      Reverse          -> 4
    )

  /** Creates a deck from the nope sauce recipe's cards, shuffled
    * @param nPlayers
    *   \- # players playing the game
    * @return
    *   created shuffled deck
    */
  def initShuffledNopeSauce(nPlayers: Int): Deck = {

    val cards = nopeSauceMap(nPlayers).flatMap { case (card, count) => List.fill(count)(card) }.toList
println(cards)
    Deck(cards).shuffled
  }

  /** Creates a new deck from another deck, shuffled
    * @param discardPile
    *   \- original deck
    * @return
    *   the new shuffled deck
    */
  def initShuffledFromDiscardPile(discardPile: Deck): Deck = {
    Deck(discardPile.cards).shuffled
  }

  /** Creates a new deck from 2 decks, adding the cards and shuffling them
    * @param drawPile
    *   \- original deck 1
    * @param discardPile
    *   \- original deck 2
    * @return
    *   created deck with all cards, shuffled
    */
  def initShuffledFromDiscardPile2(drawPile: Deck, discardPile: Deck): Deck = {
    Deck(drawPile.cards ++ discardPile.cards).shuffled
  }

  /** Removes the defuse cards and bombs from the deck
    * @param deck
    *   \- the deck to remove the cards from
    * @return
    *   tuple of card list with no bombs and defuses and the list of bombs
    */
  def removeDefuseAndBombs(deck: Deck): (List[Card], List[Card]) = {
    deck.cards.filterNot(_ == Defuse).partition({
      case ExplodingKitten => false
      case _ => true
    })
  }

}

case class Deck(private val cards: List[Card]) {

  /** Shuffles current deck
    * @return
    *   new shuffled deck
    */
  def shuffled: Deck = Deck(Random.shuffle(cards))

  /** Returns how many cards current deck has
    * @return
    *   deck length
    */
  def length: Int = cards.length

  /** Reverses current deck
    * @return
    *   new reversed deck
    */
  def reversed: Deck = Deck(cards.reverse)

  /** Returns the first N cards from the deck
    * @param n
    *   \- how many cards
    * @return
    *   a tuple with a list of the first N cards and the remaining list of cards
    */
  def getFirstN(n: Int): (List[Card], List[Card]) = {
    val (before, after) = cards.splitAt(n)
    (before, after)
  }

  /** Creates a new deck with all Exploding Kittens on the top
    * @return
    *   created deck
    */
  def withExplodingKittensOnTop: Deck = {
    val explodingKittens = cards.filter(_ == ExplodingKitten)
    val otherCards       = cards.filter(_ != ExplodingKitten)
    Deck(explodingKittens ++ otherCards)
  }

  /** If there's enough cards, draws a card for the deck
    * @return
    *   Option of tuple of the remaining deck and the card drawn
    */
  def draw: Option[(Deck, Card)] = cards match {
    case Nil             => None
    case card :: newDeck => Some(Deck(newDeck), card)
  }

  /** Creates a new deck with a card inserted at given index
    * @param index
    *   \- index to insert the card at
    * @param card
    *   \- card to insert
    * @return
    *   new deck
    */
  def insertAt(index: Int, card: Card): Deck = {
    val (before, after) = cards.splitAt(index)
    Deck(before ++ List(card) ++ after)

  }

  /** Created a new Deck with a new card on top
    * @param card
    *   \- card to prepend
    * @return
    *   new deck
    */
  def prepend(card: Card): Deck = {
    Deck(card :: cards)
  }

  /** Creates new deck with top and bottom card swapped
    * @return
    *   the new deck
    */
  def swapTopAndBottom: Deck = {
    val first +: cards :+ last = this.cards
    Deck(last +: cards :+ first)
  }

}
