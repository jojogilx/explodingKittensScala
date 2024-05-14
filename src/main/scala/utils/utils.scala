package utils

import scala.util.Random

object utils {
  def getRandomFromList[T](list: List[T]): T = {
    val random = Random.nextInt(list.length)
    list(random)
  }
}
