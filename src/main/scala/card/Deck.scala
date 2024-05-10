package card

import scala.util.Random

object Deck {


  /** Creates a deck from the recipe's cards
    * @param nPlayers
    *   \- # players playing the game
    * @return
    *   created deck
    */
  def initFromRecipe(recipe: Recipe, nPlayers: Int): Deck =
        Deck(recipe.cardCount(nPlayers).flatMap { case (card, count) => List.fill(count)(card) }.toList)


  /** Removes the defuse cards and bombs from the deck
    * @param deck
    *   \- the deck to remove the cards from
    * @return
    *   tuple of card list with no bombs and defuses and the list of bombs
    */
  def removeDefuseAndBombs(deck: Deck, numberOfDefuses: Int): (List[Card], List[Card]) = {
    val (deck2, bombs) = deck.cards.filterNot(_==Defuse).partition({
      case ExplodingKitten => false
      case _ => true
    })

    val defusesInDeck = deck.cards.count(_ == Defuse) - numberOfDefuses

    (Deck(deck2 ++ (1 to defusesInDeck).map(_ => Defuse)).shuffled.cards, bombs)
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

  /**
   * gets the top card
   * @return option of the top card
   */
  def topCard: Option[Card] = cards.headOption

  /** Returns the first N cards from the deck
    * @param n
    *   \- how many cards
    * @return
    *   a tuple with a list of the first N cards and the remaining list of cards
    */
  def getFirstN(n: Int): List[Card] = {
    val (before, _) = cards.splitAt(n)
    before
  }

  /** removes the top 3 cards
   * @return
   *   remaining cards
   */
  private def removeFirst3(): List[Card] = {
    val (_, after) = cards.splitAt(3)
   after
  }


  /** Alters the order of the top 3 cards
   * @param order order of the cards
   * @return
   *   new altered deck
   */
  def alterTheFuture3X(order: String): Deck = {
    val cardsNew  = order.map(_.toString.toInt - 1).map(getFirstN(3)).toList
    val remainingDeck = removeFirst3()
    Deck(cardsNew ++ remainingDeck)
  }

  /** Creates a new deck with all Exploding Kittens on the top
    * @return
    *   created deck
    */
  def withExplodingKittensOnTop: Deck = {
    val explodingKittens = cards.filter(_ == ExplodingKitten)
    val otherCards       = cards.filter(_ != ExplodingKitten)
    Deck(explodingKittens ++ Random.shuffle(otherCards))
  }

  /** If there's enough cards, draws a card for the deck
    * @return
    *   Option of tuple of the remaining deck and the card drawn
    */
  def draw: (Deck, Card) = cards match {
    case card :: newDeck => (Deck(newDeck), card)
  }



  /** If there's enough cards, draws a card for the bottom of the deck deck
   * @return
   *   Option of tuple of the remaining deck and the card drawn
   */
  def drawFromBottom: (Deck, Card) = cards match {
    case newDeck :+ card  => (Deck(newDeck), card)
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

  /** Created a new Deck with new cards on top
   * @param cardsN
   *   \- cards to concat
   * @return
   *   new deck
   */
  def concat(cardsN: List[Card]): Deck = {
    Deck(cardsN ++ cards)
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
