package card

import io.circe.syntax.EncoderOps
import io.circe.{Encoder, Json}

import scala.concurrent.duration.{Duration, DurationInt}



sealed trait Recipe {
  val name: String
  val description: String
  val cardCount: Int => Map[Card, Int]
  val minPlayers: Int
  val maxPlayers: Int
  val defusesOnStart: Int
  val playTime: Duration
  val cardsOnStart: Int => Int
  val available: Boolean

  override def toString: String = s"$name ($minPlayers-$maxPlayers players) - ${playTime.toMinutes.toInt.toString} min\n$description.\nCards: $getCardList"

  private def getCardList: String =
    cardCount(1000).map {
      case (card, i) if i > 900 => s"$card x (# players${
        val nP = i - 1000
        if (nP == 0) ")" else if (nP > 0) s" + $nP)" else s" $nP)"
      }"
      case (card, i) => s"$card x $i"

    }.mkString("[",", ","]")

  def getCardMap: List[(Card, String)]

}

case object NopeSauce extends Recipe {
  override val cardCount: Int => Map[Card, Int] = `# Players` =>
    Map(
      ExplodingKitten  -> (`# Players` - 1),
      Defuse           -> `# Players`,
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
  override val cardsOnStart: Int => Int = `# Players` => if(`# Players`==5) 5 else 7
  override val name: String = "Nope Sauce"
  override val description: String = "A game that's not going to go the way you planned."

  override def getCardMap: List[(Card, String)] =
    List(
    ExplodingKitten  -> "# Players - 1",
    Defuse           -> "# Players",
    Nope             -> "8",
    Shuffle          -> "4",
    Skip             -> "4",
    AlterTheFuture3X -> "2",
    SwapTopAndBottom -> "2",
    Attack2X         -> "3",
    TargetedAttack2X -> "2",
    CatomicBomb      -> "1",
    Bury             -> "3",
    Tacocat          -> "4",
    FeralCat         -> "6",
    Reverse          -> "4"
  )

  override val available: Boolean = true
}

case object AttackOfTheAttacks extends Recipe {
  override val cardCount: Int => Map[Card, Int] =
    `# Players` =>
      Map(
        ExplodingKitten -> (`# Players` - 1),
        Defuse -> (`# Players` + 1),
        Attack2X -> 4,
        TargetedAttack2X -> {
          if (`# Players` == 5) 6 else 4
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
  override val available: Boolean = false
  override val minPlayers: Int = 2
  override val maxPlayers: Int = 5
  override val defusesOnStart: Int = 1
  override val playTime: Duration = 15.minutes
  override val cardsOnStart: Int => Int = `# Players` => if(`# Players`==maxPlayers) 5 else 7
  override val name: String = "Attack of the Attacks"
  override val description: String = "How many turns in a row can you survive?"

  override def getCardMap: List[(Card, String)] =
      List(
        ExplodingKitten -> "# Players - 1",
        Defuse -> "# Players + 1",
        Attack2X -> "4",
        TargetedAttack2X -> "2-4 players: 4; 5 players: 6",
        Skip -> "4",
        SuperSkip -> "2",
        Reverse -> "3",
        SeeTheFuture3X -> "3",
        Tacocat -> "4",
        RainbowRalphingCat -> "4",
        ZombieCat -> "4",
        Nope -> "4",
        CatomicBomb -> "1"
      )
}


case object BlackHole extends Recipe {
  override val cardCount: Int => Map[Card, Int] =
    `# Players` =>
      Map(
        ImplodingKitten(false) -> (`# Players` - 1),
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
  override def getCardMap: List[(Card, String)] =
    List(
      ImplodingKitten(false) -> "# Players - 1",
      Skip -> "5",
      Nope -> "4",
      Shuffle -> "5",
      AlterTheFuture3X -> "3",
      ShareTheFuture3X -> "4",
      Reverse -> "4",
      SwapTopAndBottom -> "3",
      DrawFromTheBottom -> "3",
      Tacocat -> "3",
      RainbowRalphingCat -> "3",
      FeralCat -> "4"
    )

  override val available: Boolean = false
  override val minPlayers: Int = 3
  override val maxPlayers: Int = 5
  override val defusesOnStart: Int = 0
  override val playTime: Duration = 15.minutes
  override val cardsOnStart: Int => Int = `# Players` => if(`# Players`==maxPlayers) 5 else 7
  override val name: String = "Black Hole"
  override val description: String = "A game with only Imploding Kittens"
}

case object DangerMode extends Recipe {
  override val cardCount: Int => Map[Card, Int] =
    `# Players` =>
      Map(
        ExplodingKitten -> 6,
        Defuse -> `# Players`,
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

  override def getCardMap: List[(Card, String)] =
    List(
      ExplodingKitten -> "6",
      Defuse -> "# Players",
      Attack2X -> "4",
      TargetedAttack2X -> "4",
      SuperSkip -> "1",
      Shuffle -> "3",
      Reverse -> "5",
      DrawFromTheBottom -> "2",
      StreakingKitten -> "1",
      Tacocat -> "4",
      RainbowRalphingCat -> "4",
      ZombieCat -> "4",
      FeralCat -> "4"
    )

  override val available: Boolean = false
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
    `# Players` =>
      Map(
        ExplodingKitten -> (`# Players` - 1),
        Defuse -> (`# Players` + 1),
        Nope -> {if(`# Players`==minPlayers) 3 else 5},
        Attack2X -> {if(`# Players`==minPlayers) 2 else 4},
        Skip -> {if(`# Players`==minPlayers) 3 else 4},
        Shuffle -> {if(`# Players`==minPlayers) 2 else 4},
        SeeTheFuture3X -> {if(`# Players`==minPlayers) 2 else 5},
        ZombieCat -> {if(`# Players`==minPlayers) 0 else 4},
        Tacocat -> 4,
        BeardCat -> 4,
        RainbowRalphingCat -> 4,
      )
  override def getCardMap: List[(Card, String)] =
    List(
      ExplodingKitten -> "# Players - 1",
      Defuse -> "# Players + 1",
      Nope -> "3 Players: 3; 4-5 Players: 5",
      Attack2X -> "3 Players: 2; 4-5 Players: 4",
      Skip -> "3 Players: 3; 4-5 Players: 4",
      Shuffle -> "3 Players: 2; 4-5 Players: 4",
      SeeTheFuture3X -> "3 Players: 2; 4-5 Players: 5",
      ZombieCat -> "3 Players: 0; 4-5 Players: 4",
      Tacocat -> "4",
      BeardCat -> "4",
      RainbowRalphingCat -> "4"
    )

  override val available: Boolean = false
  override val minPlayers: Int = 2
  override val maxPlayers: Int = 5
  override val defusesOnStart: Int = 1
  override val playTime: Duration = 15.minutes
  override val cardsOnStart: Int => Int = `# Players` => if(`# Players`==maxPlayers) 5 else 7
  override val name: String = "Exploding Kittens Classic Mode"
  override val description: String = "Slight optimization of the original Exploding Kittens."
}

case object EyeForAnEye extends Recipe {
  override val cardCount: Int => Map[Card, Int] =
    `# Players` =>
      Map(
        ExplodingKitten -> (`# Players` -2),
        Defuse -> (`# Players` + 1),
        ImplodingKitten(false) -> 1,
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
  override def getCardMap: List[(Card, String)] =
    List(
      ExplodingKitten -> "# Players - 2",
      Defuse -> "# Players + 1",
      ImplodingKitten(false) -> "1",
      SeeTheFuture3X -> "5",
      AlterTheFuture3X -> "3",
      AlterTheFuture3XNOW -> "2",
      ShareTheFuture3X -> "4",
      Mark -> "4",
      Shuffle -> "2",
      Tacocat -> "4",
      RainbowRalphingCat -> "4",
      ZombieCat -> "4",
      Attack2X -> "4",
      Skip -> "4"
    )

  override val available: Boolean = false
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
    `# Players` =>
      Map(
        ExplodingKitten -> (`# Players` - 1),
        Defuse -> `# Players`,
        Skip -> 4,
        Reverse -> 2,
        Nope -> 2
      )
  override def getCardMap: List[(Card, String)] =
    List(
      ExplodingKitten -> "# Players - 1",
      Defuse -> "# Players",
      Skip -> "4",
      Reverse -> "2",
      Nope -> "2"
    )

  override val available: Boolean = true
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
    `# Players` =>
      Map(
        ExplodingKitten -> `# Players`,
        Defuse -> (`# Players` + 3),
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
  override def getCardMap: List[(Card, String)] =
    List(
      ExplodingKitten -> "# Players",
      Defuse -> "# Players + 3",
      PersonalAttack3X -> "4",
      IllTakeThat -> "2",
      Reverse -> "3",
      AlterTheFuture3XNOW -> "2",
      ShareTheFuture3X -> "3",
      GarbageCollection -> "2",
      Tacocat -> "4",
      RainbowRalphingCat -> "4",
      FeralCat -> "6",
      StreakingKitten -> "1"
    )

  override val available: Boolean = false
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
    `# Players` =>
      Map(
        ExplodingKitten -> (`# Players` - 1),
        Defuse -> (`# Players` + 2),
        ImplodingKitten(false) -> 1,
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
  override def getCardMap: List[(Card, String)] =
    List(
      ExplodingKitten -> "# Players - 1",
      Defuse -> "# Players + 2",
      ImplodingKitten(false) -> "1",
      StreakingKitten -> "1",
      Nope -> "4",
      SeeTheFuture3X -> "3",
      ShareTheFuture3X -> "4",
      Attack2X -> "4",
      Bury -> "2",
      DrawFromTheBottom -> "2",
      Skip -> "4",
      SuperSkip -> "2",
      Tacocat -> "4",
      FeralCat -> "6",
      GarbageCollection -> "2"
    )

  override val available: Boolean = false
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
    `# Players` =>
      Map(
        ExplodingKitten -> (`# Players` - 1),
        Defuse -> (`# Players` + 1),
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
  override def getCardMap: List[(Card, String)] =
    List(
      ExplodingKitten -> "# Players - 1",
      Defuse -> "# Players + 1",
      Mark -> "4",
      ShareTheFuture3X -> "4",
      Skip -> "4",
      IllTakeThat -> "4",
      Tacocat -> "4",
      RainbowRalphingCat -> "4",
      ZombieCat -> "4",
      FeralCat -> "4",
      BarkingKitten -> "2"
    )

  override val available: Boolean = false
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
    `# Players` =>
      Map(
        ExplodingKitten -> `# Players`,
        Defuse -> (`# Players` + 1),
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
  override def getCardMap: List[(Card, String)] =
    List(
      ExplodingKitten -> "# Players",
      Defuse -> "# Players + 1",
      Attack2X -> "4",
      Skip -> "4",
      Mark -> "4",
      IllTakeThat -> "4",
      StreakingKitten -> "1",
      Tacocat -> "4",
      RainbowRalphingCat -> "4",
      ZombieCat -> "4",
      BeardCat -> "4",
      FeralCat -> "6"
    )

  override val available: Boolean = false
  override val minPlayers: Int = 2
  override val maxPlayers: Int = 5
  override val defusesOnStart: Int = 1
  override val playTime: Duration = 15.minutes
  override val cardsOnStart: Int => Int = `# Players` => if(`# Players`==maxPlayers) 5 else 7
}

case object ThePurrage extends Recipe {
  override val name: String = "The  Purrage"
  override val description: String = "A game of betrayal as created by Smosh Games!"
  override val cardCount: Int => Map[Card, Int] =
    `# Players` =>
      Map(
        ExplodingKitten -> (`# Players` - 1),
        Defuse -> `# Players`,
        ImplodingKitten(false) -> 1,
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
  override def getCardMap: List[(Card, String)] =
    List(
      ExplodingKitten -> "# Players - 1",
      Defuse -> "# Players",
      ImplodingKitten(false) -> "1",
      CatomicBomb -> "1",
      SuperSkip -> "2",
      Skip -> "5",
      Reverse -> "5",
      Attack2X -> "6",
      TargetedAttack2X -> "4",
      PersonalAttack3X -> "4",
      Shuffle -> "5",
      Nope -> "8"
    )

  override val available: Boolean = false
  override val minPlayers: Int = 3
  override val maxPlayers: Int = 5
  override val defusesOnStart: Int = 1
  override val playTime: Duration = 10.minutes
  override val cardsOnStart: Int => Int = `# Players` => if(`# Players`==maxPlayers) 5 else 7
}

case object MindGames extends Recipe {
  override val name: String = "Mind Games"
  override val description: String = "You know so much, but you can do so little."
  override val cardCount: Int => Map[Card, Int] =
    `# Players` =>
      Map(
        ExplodingKitten -> (`# Players` - 1),
        Defuse -> `# Players`,
        Bury -> 4,
        SeeTheFuture3X -> 6,
        Skip -> 4,
        Mark -> 4,
        DrawFromTheBottom -> 2,
        Reverse -> 4,
        Tacocat -> 4,
        RainbowRalphingCat -> 4,
        ZombieCat -> 4
      )
  override def getCardMap: List[(Card, String)] =
    List(
      ExplodingKitten -> "# Players - 1",
      Defuse -> "# Players",
      Bury -> "4",
      SeeTheFuture3X -> "6",
      Skip -> "4",
      Mark -> "4",
      DrawFromTheBottom -> "2",
      Reverse -> "4",
      Tacocat -> "4",
      RainbowRalphingCat -> "4",
      ZombieCat -> "4"
    )
  override val available: Boolean = false
  override val minPlayers: Int = 2
  override val maxPlayers: Int = 5
  override val defusesOnStart: Int = 1
  override val playTime: Duration = 15.minutes
  override val cardsOnStart: Int => Int = _ => 5
}

case object CardHoarders extends Recipe {
  override val name: String = "Card Hoarders"
  override val description: String = "To draw, or not to draw? (The answer is to draw.)"
  override val cardCount: Int => Map[Card, Int] =
    `# Players` =>
      Map(ExplodingKitten -> {if(`# Players` == maxPlayers) 2 else 1},
        Defuse -> `# Players`,
        ImplodingKitten(false) -> 1,
        SeeTheFuture3X -> 4,
        ShareTheFuture3X -> 4,
        SwapTopAndBottom -> 3,
        DrawFromTheBottom -> 2,
        Shuffle -> 3,
        Tacocat -> 4,
        RainbowRalphingCat -> 4,
        ZombieCat -> 4,
        Nope -> 4)
  override def getCardMap: List[(Card, String)] =
    List(
      ExplodingKitten -> "2-3 Players: 1; 4 Players: 2",
      Defuse -> "# Players",
      ImplodingKitten(false) -> "1",
      SeeTheFuture3X -> "4",
      ShareTheFuture3X -> "4",
      SwapTopAndBottom -> "3",
      DrawFromTheBottom -> "2",
      Shuffle -> "3",
      Tacocat -> "4",
      RainbowRalphingCat -> "4",
      ZombieCat -> "4",
      Nope -> "4"
    )

  override val available: Boolean = false
  override val minPlayers: Int = 2
  override val maxPlayers: Int = 4
  override val defusesOnStart: Int = 1
  override val playTime: Duration = 10.minutes
  override val cardsOnStart: Int => Int = _ => 5
}

case object CatFight extends Recipe {
  override val name: String = "Cat Fight"
  override val description: String = "This just got personal."
  override val cardCount: Int => Map[Card, Int] =
    `# Players` =>
      Map(ExplodingKitten -> (`# Players` - 1),
        Defuse -> `# Players`,
        Attack2X -> 4,
        TargetedAttack2X -> 2,
        Nope -> 3,
        AlterTheFuture3XNOW -> 2,
        Reverse -> 3)
  override def getCardMap: List[(Card, String)] =
    List(
      ExplodingKitten -> "# Players - 1",
      Defuse -> "# Players",
      Attack2X -> "4",
      TargetedAttack2X -> "2",
      Nope -> "3",
      AlterTheFuture3XNOW -> "2",
      Reverse -> "3"
    )
  override val available: Boolean = false
  override val minPlayers: Int = 2
  override val maxPlayers: Int = 4
  override val defusesOnStart: Int = 1
  override val playTime: Duration = 5.minutes
  override val cardsOnStart: Int => Int = _ => 2
}

object Recipes {
  val recipesList: List[Recipe] = List(AttackOfTheAttacks, BlackHole, CardHoarders, CatFight, DangerMode, ExplodingKittensClassicMode, EyeForAnEye, LightningKittens, Meowsochist, MindGames, NopeSauce, PowerPlay, SharingIsCaring, StickyFingers, ThePurrage)

  implicit val RecipeEncoder: Encoder[Recipe] = recipe =>
    Json.obj(
      "name"             -> recipe.name.asJson,
      "description"      -> recipe.description.asJson,
      "min_players"      -> recipe.minPlayers.asJson,
      "max_players"      -> recipe.maxPlayers.asJson,
      "defuses_on_start" -> recipe.defusesOnStart.asJson,
      "duration"         -> recipe.playTime.toMinutes.asJson,
      "cards"            -> recipe.getCardMap.asJson,
      "available" -> recipe.available.asJson
    )

  /**
   * finds Recipe with name
   * @param n name of the recioe
   * @return option of recipe
   */
  def getRecipe(n: String): Option[Recipe] = recipesList.find(_.name == n)

}