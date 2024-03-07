package card

object Cards {

  type Hand = List[Card]

  sealed trait Card {
    val description: String
    val title: String

  }

  case class ExplodingKitten() extends Card {
    override val description: String = "Show this card immediately"
    override val title: String = "Exploding Kitten"
  }


  case class Defuse() extends Card {
    override val description: String = "Instead of exploding, put your last drawn card back into the deck"
    override val title: String = "Defuse"
  }

  case class Shuffle() extends Card {
    override val description: String = "Shuffle the draw pile"
    override val title: String = "Shuffle"
  }

  case class Nope() extends Card {
    override val description: String = "Stop the action of another player. You can play this at any time"
    override val title: String = "Nope"
  }

  case class Skip() extends Card {
    override val description: String = "End turn without drawing a card"
    override val title: String = "Skip"
  }

  case class AlterTheFuture3X() extends Card {
    override val description: String = "Privately view and rearrange the top three cards of the draw pile"
    override val title: String = "Alter The Future (3X)"
  }

  case class SwapTopAndBottom() extends Card {
    override val description: String = "Swap top and bottom cards from the draw pile"
    override val title: String = "Swap Top And Bottom"
  }

  case class Attack2X() extends Card {
    override val description: String = "End your turn without drawing a card. Force the next player to take two turns"
    override val title: String = "Attack (2X)"
  }

  case class TargetedAttack2X() extends Card {
    override val description: String = "End your turn without drawing a card. " +
      "Force one player to take two turns. Game continues from that player"
    override val title: String = "Targeted Attack (2X)"
  }

  case class CatomicBomb() extends Card {
    override val description: String = "Remove the exploding kittens from the deck. " +
      "Put all the kittens top of the drawn pile. Your turn ends after playing this card"
    override val title: String = "Catomic Bomb"
  }

  case class Bury() extends Card {
    override val description: String = "End your turn by putting the next card you draw back into the draw pile as if you had defused it"
    override val title: String = "Bury"
  }

  case class Tacocat() extends Card {
    override val description: String = "This is a cat card and is powerless"
    /*+" on its own. Play two of the same cats as a pair to steal a random card from another player"*/
    override val title: String = "Tacocat"
  }

  case class FeralCat() extends Card {
    override val description: String = "Use as any cat card"
    override val title: String = "Feral Cat"
  }

  case class Reverse() extends Card {
    override val description: String = "Reverse the order of play and end your turn without drawing a card"
    override val title: String = "Reverse"
  }


}
