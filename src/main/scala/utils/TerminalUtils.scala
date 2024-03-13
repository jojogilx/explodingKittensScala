package utils

import players.Player

import scala.language.implicitConversions

object TerminalUtils {
  val ResetText = "\u001b[0m"

  val BlackText        = "\u001b[30m"
  val RedText          = "\u001b[31m"
  val GreenText        = "\u001b[32m"
  val YellowText       = "\u001b[33m"
  val BrightYellowText = "\u001b[93m"
  val BlueText         = "\u001b[34m"
  val MagentaText      = "\u001b[35m"
  val CyanText         = "\u001b[36m"
  val WhiteText        = "\u001b[97m"
  val OrangeText       = "\u001B[38;5;208m"
  val GreyText         = "\u001b[37m"
  val BrightGreenText  = "\u001b[92m"
  val PinkText         = "\u001b[38;5;206m"

  val RedBackground     = "\u001B[41m"
  val GreenBackground   = "\u001B[42m"
  val YellowBackground  = "\u001B[43m"
  val BlueBackground    = "\u001B[44m"
  val MagentaBackground = "\u001B[45m"
  val CyanBackground    = "\u001B[46m"
  val WhiteBackground   = "\u001B[107m"
  val OrangeBackground  = "\u001B[48;5;208m"

  val Bold      = "\u001b[1m"
  val Underline = "\u001b[4m"

  val BombEmojiUnicode: String    = "\uD83D\uDCA3"
  val FireEmojiUnicode: String    = "\uD83D\uDD25"
  val SkullEmojiUnicode: String   = "\u2620\uFE0F"
  val CatFaceEmojiUnicode: String = "\uD83D\uDC31"
  val CatBodyEmojiUnicode: String = "\uD83D\uDC08"

  val gameTitleBanner: String = s"$CatFaceEmojiUnicode$CatBodyEmojiUnicode$OrangeText$Bold " +
    s"EXPLODING$ResetText$Bold KITTENS$ResetText - ${RedText}Scala Edition$ResetText " +
    s"$BombEmojiUnicode$FireEmojiUnicode\n\n"


  private val SystemMessageColor = CyanText
  private val ErrorColor = RedText

  val PlayerColors: List[String] =
    List(RedText, BlueText, YellowText, GreenText, MagentaText)


  def colorSystemMessage(message: String): String = colorMessage(SystemMessageColor,message)
  def colorMessage(color: String, message: String): String = s"$color$message$ResetText"
  def colorPlayerMessage(player: Player, message: String): String = s"${player.color}${player.playerID}$ResetText$message"

  def colorErrorMessage(error: String): String = colorMessage(ErrorColor, error)

  def diedMessage(player: Player): String = s"\n$SkullEmojiUnicode " +
    s"${colorPlayerMessage(player, " died")} $SkullEmojiUnicode\n"


  val TurnSeparator: String = "-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-"

}
