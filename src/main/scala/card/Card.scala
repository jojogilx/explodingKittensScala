package card

import io.circe.generic.semiauto.deriveDecoder
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, Json}
sealed trait Card {
  val description: String
  val title: String
}

sealed trait CatCard extends Card {
  override val description: String = s"This is a cat card and is powerless on its own. Play two ${title}s to steal a random card from another player or 3 to name a card you want from them"
}

sealed trait Now
sealed trait Skipper


case object ExplodingKitten extends Card {
  override val description: String = "Show this card immediately"
  override val title: String       = "Exploding Kitten"
}

case object Defuse extends Card {
  override val description: String = "Instead of exploding, put your last drawn card back into the deck"
  override val title: String       = "Defuse"
}

case object Shuffle extends Card {
  override val description: String = "Shuffle the draw pile"
  override val title: String       = "Shuffle"
}

case object Nope extends Card with Now {
  override val description: String = "Stop the action of another player. You can play this at any time"
  override val title: String       = "Nope"
}

case object Skip extends Card with Skipper {
  override val description: String = "End turn without drawing a card"
  override val title: String       = "Skip"
}

case object AlterTheFuture3X extends Card {
  override val description: String = "Privately view and rearrange the top three cards of the draw pile"
  override val title: String       = "Alter The Future (3X)"
}

case object SwapTopAndBottom extends Card {
  override val description: String = "Swap top and bottom cards from the draw pile"
  override val title: String       = "Swap Top And Bottom"
}

case object Attack2X extends Card with Skipper {
  override val description: String = "End your turn without drawing a card. Force the next player to take two turns"
  override val title: String       = "Attack (2X)"
}

case object TargetedAttack2X extends Card with Skipper {
  override val description: String = "End your turn without drawing a card. " +
    "Force one player to take two turns. Game continues from that player"
  override val title: String     = "Targeted Attack (2X)"
}

case object CatomicBomb extends Card with Skipper {
  override val description: String = "Remove the exploding kittens from the deck. " +
    "Put all the kittens top of the drawn pile. Your turn ends after playing this card"
  override val title: String     = "Catomic Bomb"
}

case object Bury extends Card {
  override val description: String =
    "End your turn by putting the next card you draw back into the draw pile as if you had defused it"
  override val title: String = "Bury"
}
  case object Tacocat extends Card {
    override val description: String = "This is a cat card and is powerless" +
      " on its own. Play two of the same cats as a pair to steal a random card from another player or 3 to chose the card you want from them"
    override val title: String = "Tacocat"
  }

  case object FeralCat extends Card {
    override val description: String = "Use as any cat card"
    override val title: String = "Feral Cat"
  }

  case object Reverse extends Card with Skipper {
    override val description: String = "Reverse the order of play and end your turn without drawing a card"
    override val title: String = "Reverse"
  }


  case object AlterTheFuture3XNOW extends Card with Now {
    override val description: String = "Privately view and rearrange the top three cards of the draw pile. Play at any time"
    override val title: String = "Alter The Future (3X) NOW"
  }

  case object BarkingKitten extends Card {
    override val description: String = "When played, if anyone has another Barking Kitten they explode (or Defuse). Otherwise keep it in front of you, and it becomes a target when another one is played."
    override val title: String = "Barking Kitten"
  }

  case object BeardCat extends CatCard {
    override val title: String = "Beard Cat"
  }

  case object RainbowRalphingCat extends CatCard {
    override val title: String = "Rainbow-Ralphing Cat"
  }

  case object ZombieCat extends CatCard {
    override val title: String = "Zombie Cat"
  }


  case object DrawFromTheBottom extends Card {
    override val description: String = "End your turn by drawing the bottom card from the Draw Pile"
    override val title: String = "Draw From The Bottom"
  }

  case object GarbageCollection extends Card {
    override val description: String = "Every player (Including the one who played this card) must choose one card to shuffle into the draw pile"
    override val title: String = "Garbage Collection"
  }

  case object IllTakeThat extends Card {
    override val description: String = "Put this card in front of another player to steal the next card they draw from the pile."
    override val title: String = "I'll Take That"
  }

  case class ImplodingKitten(faceUp: Boolean) extends Card {
    override val description: String = "When drawn face down, put back in the deck face up, without using a defuse. When drawn face up, explode immediately. This card cannot be defused"
    override val title: String = "Imploding Kitten"
  }

  case object Mark extends Card {
    override val description: String = "Choose a player and randomly pick one of their cards. They must keep that card facing outward in their hand until it is played or stolen"
    override val title: String = "Mark"
  }

  case object PersonalAttack3X extends Card {
    override val description: String = "Take three turns in a row"
    override val title: String = "Personal Attack 3X"
  }

  case object SeeTheFuture3X extends Card {
    override val description: String = "Privately view the top three cards of the deck"
    override val title: String = "See The Future 3X"
  }

  case object SeeTheFuture5X extends Card {
    override val description: String = "Privately view the top five cards of the deck"
    override val title: String = "See The Future 5X"
  }

  case object ShareTheFuture3X extends Card {
    override val description: String = "View and rearrange the top three cards in the draw pile, then show the cards to the next player"
    override val title: String = "Share The Future 3X"
  }

  case object StreakingKitten extends Card {
    override val description: String = "Keep this card a secret. As long as it's in your hand you may draw and secretly hold one Exploding Kitten without blowing up"
    override val title: String = "Streaking Kitten"
  }

  case object SuperSkip extends Card with Skipper {
    override val description: String = "End your turn without drawing a card. If you’re supposed to take multiple turns, end them all"
    override val title: String = "Super Skip"
  }


object Card {

  implicit val CardEncoder: Encoder[Card] = card => {
    Json.obj(
      "name" -> card.title.asJson,
      "description" -> card.description.asJson,
    )
  }

  implicit val CardDecoder: Decoder[Card] = deriveDecoder[Card]



}
