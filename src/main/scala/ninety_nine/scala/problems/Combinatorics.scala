package ninety_nine.scala.problems

object Combinatorics:
  import scala.annotation.tailrec

  /** Problem 26: Generates the combinations of [k] distinct objects chosen from
   *  [n] elements of a [list]
   */
  def getCombinations[T](k: Int, list: List[T]): List[List[T]] = 
    if k < 0 then return Nil
    def gc(k: Int, list: List[T], comb: List[T], acc: List[List[T]]): List[List[T]] = k match
      case 0 => comb :: acc
      case _ => list match
        case Nil => acc
        case head :: tail => gc(k - 1, tail, head :: comb, acc) ::: gc(k, tail, comb, acc)
    gc(k, list, Nil, Nil)
