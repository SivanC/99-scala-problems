package ninety_nine.scala.problems

/** Solutions related to duplicate elements in lists */
object Duplicates:
  import ListOps.*
  import scala.annotation.tailrec

  /** Problem 8: Returns a list without consecutive duplicates */
  def removeConsecutiveDuplicates[T](list: List[T]): List[T] =
    @tailrec
    def rcd(list: List[T], acc: List[T]): List[T] = list match
      case Nil          => acc
      case head :: Nil  => head :: acc
      case head :: tail => tail.head match
        case h1 if head == h1 => rcd(tail, acc) // current head matches next element
        case _                => rcd(tail, head :: acc) // current head is diff than next
    reverseList(rcd(list, Nil))

  /** Problem 9: Packs consecutive duplicates in a list into nested lists */
  def packConsecutiveDuplicates[T](list: List[T]): List[List[T]] =
    @tailrec
    def pcd(list: List[T], smallAcc: List[T], acc: List[List[T]]): List[List[T]] = list match
      case Nil => if smallAcc.size > 0 then smallAcc :: acc else acc
      case head :: tail if listSize(smallAcc) == 0 || smallAcc.head == head => // Nil or in a run
        pcd(tail, head :: smallAcc, acc)
      case head :: tail => pcd(tail, head :: Nil, smallAcc :: acc) // new run
    reverseList(pcd(list, Nil, Nil))

  /** Problem 14: Duplicates each element in a list in order */
  def duplicateElementsOnce[T](list: List[T]): List[T] =
    @tailrec
    def deo(list: List[T], acc: List[T]): List[T] = list match
      case Nil => acc
      case head :: tail => deo(tail, head :: head :: acc)
    reverseList(deo(list, Nil))

  /** Problem 15: Duplicate the elements of a list such that the list contains
   *  each original element [times] times, in original order. If
   *  [times] is 1 or less, return the original list. */
  def duplicateElements[T](times: Int, list: List[T]): List[T] =
    if times < 2 then return list
    @tailrec
    def de(list: List[T], acc: List[T]): List[T] = list match
      case Nil => acc
      case head :: tail => 
        de(tail, (for i <- 1 to times yield head).toList ++: acc)
        // Alternative to the guard immediately below function declaration
        // de(times, tail, head :: (for i <- 1 until times yield head).toList ++: acc)
    reverseList(de(list, Nil))

