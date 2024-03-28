package card

import scala.concurrent.duration.{Duration, DurationInt}

abstract case class Recipe() {
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
}

case object AttackOfAttacks extends Recipe {
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
  override val cardsOnStart: Int => Int = nPlayers => if(nPlayers==5) 5 else 7
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
}

case object ClassicMode extends Recipe {
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
  override val cardsOnStart: Int => Int = nPlayers => if(nPlayers==5) 5 else 7
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
