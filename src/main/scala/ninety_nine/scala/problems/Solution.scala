package ninety_nine.scala.problems

import scala.annotation.tailrec
import java.util.NoSuchElementException

/** Holds solutions to [99 Scala problems](https://aperiodic.net/pip/scala/s-99/#p01) */
object Solution:
  /** Problem 1: Returns the last element of a list, in a list */
  def getLastElement[T](list: List[T]): T =
    @tailrec
    def gla[T](list: List[T]): List[T] = list match
      case Nil => Nil
      case head :: Nil => head :: Nil
      case head :: tail => gla(tail)
    
    gla(list) match
      case Nil => throw NoSuchElementException("last of empty list")
      case List(res) => res
      case l: List[T] => l.last // shouldn't happen
