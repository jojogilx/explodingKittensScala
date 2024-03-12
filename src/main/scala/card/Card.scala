package card

import utils.TerminalUtils._

sealed trait Card {
  val description: String
  val title: String
  val emoji: String
  val textColor: String

  override def toString: String = s"$textColor $emoji $title $emoji $ResetText"

}

case object ExplodingKitten extends Card {
  override val description: String = "Show this card immediately"
  override val title: String       = "Exploding Kitten"
  override val emoji: String       = "\uD83D\uDCA3"
  override val textColor: String   = s"$OrangeBackground$BlackText"
}

case object Defuse extends Card {
  override val description: String = "Instead of exploding, put your last drawn card back into the deck"
  override val title: String       = "Defuse"
  override val emoji: String       = "\u2702\uFE0F"
  override val textColor: String   = BrightGreenText
}

case object Shuffle extends Card {
  override val description: String = "Shuffle the draw pile"
  override val title: String       = "Shuffle"
  override val emoji: String       = "\uD83D\uDD00"
  override val textColor: String   = GreyText
}

case object Nope extends Card {
  override val description: String = "Stop the action of another player. You can play this at any time"
  override val title: String       = "Nope"
  override val emoji: String       = "\uD83D\uDED1"
  override val textColor: String   = RedText
}

case object Skip extends Card {
  override val description: String = "End turn without drawing a card"
  override val title: String       = "Skip"
  override val emoji: String       = "\u23ED"
  override val textColor: String   = BlueText
}

case object AlterTheFuture3X extends Card {
  override val description: String = "Privately view and rearrange the top three cards of the draw pile"
  override val title: String       = "Alter The Future (3X)"
  override val emoji: String       = "\uD83E\uDDD9"
  override val textColor: String   = MagentaText
}

case object SwapTopAndBottom extends Card {
  override val description: String = "Swap top and bottom cards from the draw pile"
  override val title: String       = "Swap Top And Bottom"
  override val emoji: String       = "\u2194\uFE0F"
  override val textColor: String   = YellowText
}

case object Attack2X extends Card {
  override val description: String = "End your turn without drawing a card. Force the next player to take two turns"
  override val title: String       = "Attack (2X)"
  override val emoji: String       = "\u26A1"
  override val textColor: String   = OrangeText
}

case object TargetedAttack2X extends Card {
  override val description: String = "End your turn without drawing a card. " +
    "Force one player to take two turns. Game continues from that player"
  override val title: String     = "Targeted Attack (2X)"
  override val emoji: String     = "\u2623\uFE0F"
  override val textColor: String = OrangeText
}

case object CatomicBomb extends Card {
  override val description: String = "Remove the exploding kittens from the deck. " +
    "Put all the kittens top of the drawn pile. Your turn ends after playing this card"
  override val title: String     = "Catomic Bomb"
  override val emoji: String     = "\u2622\uFE0F"
  override val textColor: String = s"$BrightYellowText"
}

case object Bury extends Card {
  override val description: String =
    "End your turn by putting the next card you draw back into the draw pile as if you had defused it"
  override val title: String     = "Bury"
  override val emoji: String     = "\u26CF️"
  override val textColor: String = GreyText
}

case object Tacocat extends Card {
  override val description: String = "This is a cat card and is powerless"
  /*+" on its own. Play two of the same cats as a pair to steal a random card from another player"*/
  override val title: String     = "Tacocat"
  override val emoji: String     = "\uD83C\uDF2E"
  override val textColor: String = WhiteText
}

case object FeralCat extends Card {
  override val description: String = "Use as any cat card"
  override val title: String       = "Feral Cat"
  override val emoji: String       = "\u2753"
  override val textColor: String   = PinkText
}

case object Reverse extends Card {
  override val description: String = "Reverse the order of play and end your turn without drawing a card"
  override val title: String       = "Reverse"
  override val emoji: String       = "\u21BA"
  override val textColor: String   = BlueText
}
