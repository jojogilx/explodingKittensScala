package utils

import scala.language.implicitConversions

object Utils {

  implicit def intWithTimes(n: Int): Object {def times(f: => Unit): Unit} = new {
       def times(f: => Unit): Unit = 1 to n foreach { _ => f}
  }

  object TerminalUtils {
    val Reset       = "\u001b[0m"
    val BlackText   = "\u001b[30m"
    val RedText     = "\u001b[31m"
    val GreenText   = "\u001b[32m"
    val YellowText  = "\u001b[33m"
    val BrightYellowText  = "\u001b[93m"
    val BlueText    = "\u001b[34m"
    val MagentaText = "\u001b[35m"
    val CyanText    = "\u001b[36m"
    val WhiteText   = "\u001b[97m"
    val OrangeText  = "\u001B[38;5;208m"
    val GreyText = "\u001b[37m"
    val LimeGreenText = "\u001b[92m"
    val PinkText = "\u001b[38;5;206m"

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
  }

}
