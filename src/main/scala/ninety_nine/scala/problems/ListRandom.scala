package ninety_nine.scala.problems

object ListRandom:
  import ListOps.*
  import scala.annotation.tailrec
  import util.Random

  // generators
  given seededRng: Random = Random(99) 
  given rng: Random = Random()

  /** Problem 23: Extracts [n] random element from a list */
  def extractRandomFromList[T](n: Int, list: List[T])(using r: Random): List[T] = 
    @tailrec
    def erfl(c: Int, list: List[T], acc: List[T]): List[T] = list match
      case Nil => acc
      case head :: tail if c == n => acc
      case l: List[T] => 
        val (remained, removed) = removeKthElement(r.between(0, l.size), l)
        removed match
          case Some(rem) => erfl(c + 1, remained, rem :: acc)
          case None => acc // shouldn't happen because list is non-Nil
    erfl(0, list, Nil)

  /** Problem 24: Draws [n] different random numbers from a pool 1 to [m] */
  def drawNumbers(n: Int, m: Int)(using r: Random): List[Int] = 
    val range = if m >= 1 then listRange(1, m) else listRange(1, m, -1)
    extractRandomFromList(range.size, range)

  /** Problem 25: Generates a random permutation of a [list] */
  def listPermutation[T](list: List[T])(using Random): List[T] =
    extractRandomFromList(list.size, list)
