package card

import scala.concurrent.duration.{Duration, DurationInt}

abstract case class Recipe() {
  val name: String
  val description: String
  val cardCount: Int => Map[Card, Int]
  val minPlayers: Int
  val maxPlayers: Int
  val defusesOnStart: Int
  val playTime: Duration
  val cardsOnStart: Int => Int
}

case object NopeSauce extends Recipe {
  override val cardCount: Int => Map[Card, Int] = nPlayers =>
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

  override val minPlayers: Int = 2
  override val maxPlayers: Int = 5
  override val defusesOnStart: Int = 1
  override val playTime: Duration = 15.minutes
  override val cardsOnStart: Int => Int = nPlayers => if(nPlayers==5) 5 else 7
  override val name: String = "Nope Sauce"
  override val description: String = "A game that's not going to go the way you planned."
}

case object AttackOfTheAttacks extends Recipe {
  override val cardCount: Int => Map[Card, Int] =
    nPlayers =>
      Map(
        ExplodingKitten -> (nPlayers - 1),
        Defuse -> (nPlayers + 1),
        Attack2X -> 4,
        TargetedAttack2X -> {
          if (nPlayers == 5) 6 else 4
        },
        Skip -> 4,
        SuperSkip -> 2,
        Reverse -> 3,
        SeeTheFuture3X -> 3,
        Tacocat -> 4,
        RainbowRalphingCat -> 4,
        ZombieCat -> 4,
        Nope -> 4,
        CatomicBomb -> 1
      )

  override val minPlayers: Int = 2
  override val maxPlayers: Int = 5
  override val defusesOnStart: Int = 1
  override val playTime: Duration = 15.minutes
  override val cardsOnStart: Int => Int = nPlayers => if(nPlayers==maxPlayers) 5 else 7
  override val name: String = "Attack of the Attacks"
  override val description: String = "How many turns in a row can you survive?"
}


case object BlackHole extends Recipe {
  override val cardCount: Int => Map[Card, Int] =
    nPlayers =>
      Map(
        ImplodingKitten -> (nPlayers - 1),
        Skip -> 5,
        Nope -> 4,
        Shuffle -> 5,
        AlterTheFuture3X -> 3,
        ShareTheFuture3X -> 4,
        Reverse -> 4,
        SwapTopAndBottom ->3,
        DrawFromTheBottom -> 3,
        Tacocat -> 3,
        RainbowRalphingCat -> 3,
        FeralCat -> 4
      )
  override val minPlayers: Int = 3
  override val maxPlayers: Int = 5
  override val defusesOnStart: Int = 0
  override val playTime: Duration = 15.minutes
  override val cardsOnStart: Int => Int = nPlayers => if(nPlayers==maxPlayers) 5 else 7
  override val name: String = "Black Hole"
  override val description: String = "A game with only Imploding Kittens"
}

case object DangerMode extends Recipe {
  override val cardCount: Int => Map[Card, Int] =
    nPlayers =>
      Map(
        ExplodingKitten -> 6,
        Defuse -> nPlayers,
        Attack2X -> 4,
        TargetedAttack2X -> 4,
        SuperSkip -> 1,
        Shuffle -> 3,
        Reverse -> 5,
        DrawFromTheBottom -> 2,
        StreakingKitten -> 1,
        Tacocat -> 4,
        RainbowRalphingCat -> 4,
        ZombieCat -> 4,
        FeralCat -> 4
      )
  override val minPlayers: Int = 2
  override val maxPlayers: Int = 4
  override val defusesOnStart: Int = 1
  override val playTime: Duration = 10.minutes
  override val cardsOnStart: Int => Int = _ => 7
  override val name: String = "Danger Mode"
  override val description: String = "Warning: Highly explosive"
}

case object ExplodingKittensClassicMode extends Recipe {
  override val cardCount: Int => Map[Card, Int] =
    nPlayers =>
      Map(
        ExplodingKitten -> (nPlayers - 1),
        Defuse -> (nPlayers + 1),
        Nope -> {if(nPlayers==minPlayers) 3 else 5},
        Attack2X -> {if(nPlayers==minPlayers) 2 else 4},
        Skip -> {if(nPlayers==minPlayers) 3 else 4},
        Shuffle -> {if(nPlayers==minPlayers) 2 else 4},
        SeeTheFuture3X -> {if(nPlayers==minPlayers) 2 else 5},
        ZombieCat -> {if(nPlayers==minPlayers) 0 else 4},
        Tacocat -> 4,
        BeardCat -> 4,
        RainbowRalphingCat -> 4,
      )
  override val minPlayers: Int = 2
  override val maxPlayers: Int = 5
  override val defusesOnStart: Int = 1
  override val playTime: Duration = 15.minutes
  override val cardsOnStart: Int => Int = nPlayers => if(nPlayers==maxPlayers) 5 else 7
  override val name: String = "Exploding Kittens Classic Mode"
  override val description: String = "Slight optimization of the original Exploding Kittens."
}

case object EyeForAnEye extends Recipe {
  override val cardCount: Int => Map[Card, Int] =
    nPlayers =>
      Map(
        ExplodingKitten -> (nPlayers -2),
        Defuse -> (nPlayers + 1),
        ImplodingKitten -> 1,
        SeeTheFuture3X -> 5,
        AlterTheFuture3X -> 3,
        AlterTheFuture3XNOW -> 2,
        ShareTheFuture3X -> 4,
        Mark -> 4,
        Shuffle -> 2,
        Tacocat -> 4,
        RainbowRalphingCat -> 4,
        ZombieCat -> 4,
        Attack2X -> 4,
        Skip -> 4
      )
  override val minPlayers: Int = 3
  override val maxPlayers: Int = 5
  override val defusesOnStart: Int = 1
  override val playTime: Duration = 15.minutes
  override val cardsOnStart: Int => Int = _ => 7
  override val name: String = "Eye for an Eye"
  override val description: String = "A game where you can usually see the horrible things about to happen to you."
}

case object LightningKittens extends Recipe {
  override val cardCount: Int => Map[Card, Int] =
    nPlayers =>
      Map(
        ExplodingKitten -> (nPlayers - 1),
        Defuse -> nPlayers,
        Skip -> 4,
        Reverse -> 2,
        Nope -> 2
      )
  override val minPlayers: Int = 2
  override val maxPlayers: Int = 4
  override val defusesOnStart: Int = 1
  override val playTime: Duration = 2.minutes
  override val cardsOnStart: Int => Int = _ => 1
  override val name: String = "Lightning Kittens"
  override val description: String = "A 2 minute game"
}

case object Meowsochist extends Recipe {
  override val cardCount: Int => Map[Card, Int] =
    nPlayers =>
      Map(
        ExplodingKitten -> nPlayers,
        Defuse -> (nPlayers + 3),
        PersonalAttack3X -> 4,
        IllTakeThat -> 2,
        Reverse -> 3,
        AlterTheFuture3XNOW -> 2,
        ShareTheFuture3X -> 3,
        GarbageCollection -> 2,
        Tacocat -> 4,
        RainbowRalphingCat -> 4,
        FeralCat -> 6,
        StreakingKitten -> 1
      )
  override val minPlayers: Int = 2
  override val maxPlayers: Int = 5
  override val defusesOnStart: Int = 1
  override val playTime: Duration = 10.minutes
  override val cardsOnStart: Int => Int = _ => 5
  override val name: String = "Meowsochist"
  override val description: String = "A game with a lot of risk, and a lot of reward"
}

case object PowerPlay extends Recipe {
  override val cardCount: Int => Map[Card, Int] =
    nPlayers =>
      Map(
        ExplodingKitten -> (nPlayers - 1),
        Defuse -> (nPlayers + 2),
        ImplodingKitten -> 1,
        StreakingKitten -> 1,
        Nope -> 4,
        SeeTheFuture3X -> 3,
        ShareTheFuture3X -> 4,
        Attack2X -> 4,
        Bury -> 2,
        DrawFromTheBottom -> 2,
        Skip -> 4,
        SuperSkip -> 2,
        Tacocat -> 4,
        FeralCat -> 6,
        GarbageCollection -> 2
      )
  override val minPlayers: Int = 3
  override val maxPlayers: Int = 5
  override val defusesOnStart: Int = 1
  override val playTime: Duration = 15.minutes
  override val cardsOnStart: Int => Int = _ => 7
  override val name: String = "Power Play"
  override val description: String = "A game where there are no small moves"
}

case object SharingIsCaring extends Recipe {
  override val name: String = "Sharing Is Caring"
  override val description: String = "What's mine is yours... even the bad stuff."
  override val cardCount: Int => Map[Card, Int] =
    nPlayers =>
      Map(
        ExplodingKitten -> (nPlayers - 1),
        Defuse -> (nPlayers + 1),
        Mark -> 4,
        ShareTheFuture3X -> 4,
        Skip -> 4,
        IllTakeThat -> 4,
        Tacocat -> 4,
        RainbowRalphingCat -> 4,
        ZombieCat -> 4,
        FeralCat -> 4,
        BarkingKitten -> 2
      )
  override val minPlayers: Int = 3
  override val maxPlayers: Int = 5
  override val defusesOnStart: Int = 1
  override val playTime: Duration = 10.minutes
  override val cardsOnStart: Int => Int = _ => 5
}

case object StickyFingers extends Recipe {
  override val name: String = "Sticky Fingers"
  override val description: String = "A game where stealing, thievery and general criminality are rewarded."
  override val cardCount: Int => Map[Card, Int] =
    nPlayers =>
      Map(
        ExplodingKitten -> nPlayers,
        Defuse -> (nPlayers + 1),
        Attack2X -> 4,
        Skip -> 4,
        Mark -> 4,
        IllTakeThat -> 4,
        StreakingKitten -> 1,
        Tacocat -> 4,
        RainbowRalphingCat -> 4,
        ZombieCat -> 4,
        BeardCat -> 4,
        FeralCat -> 6,

      )
  override val minPlayers: Int = 2
  override val maxPlayers: Int = 5
  override val defusesOnStart: Int = 1
  override val playTime: Duration = 15.minutes
  override val cardsOnStart: Int => Int = nPlayers => if(nPlayers==maxPlayers) 5 else 7
}

case object SharingIsCaring2P extends Recipe {
  override val name: String = "Sharing Is Caring"
  override val description: String = "What's mine is yours... even the bad stuff, but for two players."
  override val cardCount: Int => Map[Card, Int] =
    _ =>
      Map(
        ExplodingKitten -> 1,
        Defuse -> 3,
        Mark -> 2,
        ShareTheFuture3X -> 2,
        Skip -> 3,
        IllTakeThat -> 1,
        Tacocat -> 2,
        RainbowRalphingCat -> 2,
        FeralCat -> 4,
        BarkingKitten -> 2
      )
  override val minPlayers: Int = 2
  override val maxPlayers: Int = 2
  override val defusesOnStart: Int = 1
  override val playTime: Duration = 10.minutes
  override val cardsOnStart: Int => Int = _ => 7
}

case object PowerPlay2P extends Recipe {
  override val cardCount: Int => Map[Card, Int] =
    _ =>
      Map(
        ExplodingKitten -> 1,
        Defuse -> 3,
        ImplodingKitten -> 1,
        StreakingKitten -> 1,
        Nope -> 2,
        SeeTheFuture3X -> 1,
        ShareTheFuture3X -> 1,
        Attack2X -> 2,
        DrawFromTheBottom -> 1,
        Skip -> 1,
        SuperSkip -> 1,
        Tacocat -> 2,
        FeralCat -> 4,
        GarbageCollection -> 1
      )
  override val minPlayers: Int = 2
  override val maxPlayers: Int = 2
  override val defusesOnStart: Int = 1
  override val playTime: Duration = 10.minutes
  override val cardsOnStart: Int => Int = _ => 7
  override val name: String = "Power Play"
  override val description: String = "A game where there are no small moves, but for 2 players"
}

case object BlackHole2P extends Recipe {
  override val cardCount: Int => Map[Card, Int] =
    _ =>
      Map(
        ImplodingKitten -> 1,
        Skip -> 3,
        Nope -> 3,
        Shuffle -> 2,
        AlterTheFuture3X -> 2,
        SwapTopAndBottom ->1,
        DrawFromTheBottom -> 1,
        Tacocat -> 2,
        RainbowRalphingCat -> 2,
        FeralCat -> 4
      )
  override val minPlayers: Int = 3
  override val maxPlayers: Int = 5
  override val defusesOnStart: Int = 0
  override val playTime: Duration = 15.minutes
  override val cardsOnStart: Int => Int = nPlayers => if(nPlayers==maxPlayers) 5 else 7
  override val name: String = "Black Hole"
  override val description: String = "A game with only Imploding Kittens, but for 2 players"
}

case object ThePurrage extends Recipe {
  override val name: String = "The  Purrage"
  override val description: String = "A game of betrayal as created by Smosh Games!"
  override val cardCount: Int => Map[Card, Int] =
    nPlayers =>
      Map(
        ExplodingKitten -> (nPlayers - 1),
        Defuse -> nPlayers,
        ImplodingKitten -> 1,
        CatomicBomb -> 1,
        SuperSkip -> 2,
        Skip -> 5,
        Reverse -> 5,
        Attack2X -> 6,
        TargetedAttack2X -> 4,
        PersonalAttack3X -> 4,
        Shuffle -> 5,
        Nope -> 8
      )
  override val minPlayers: Int = 3
  override val maxPlayers: Int = 5
  override val defusesOnStart: Int = 1
  override val playTime: Duration = 10.minutes
  override val cardsOnStart: Int => Int = nPlayers => if(nPlayers==maxPlayers) 5 else 7
}

object Recipes {

  val nopeSauceMap: Int => Map[Card, Int] = nPlayers =>
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



}
