package utils

import scala.language.implicitConversions

object Utils {

  implicit def intWithTimes(n: Int): Object {def times(f: => Unit): Unit} = new {
       def times(f: => Unit): Unit = 1 to n foreach { _ => f}
  }

}
