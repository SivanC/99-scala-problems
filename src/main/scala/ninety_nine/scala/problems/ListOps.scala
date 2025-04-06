package ninety_nine.scala.problems

/** Solutions related to abstract manipulation of lists */
object ListOps:
  import scala.annotation.tailrec
  import java.util.NoSuchElementException

  /** Problem 1: Returns the last element of a list. Errors if given empty list */
  def getLastElement[T](list: List[T]): T =
    @tailrec
    def gle[T](list: List[T]): List[T] = list match
      case Nil => throw NoSuchElementException("last of empty list")
      case head :: Nil => head :: Nil
      case head :: tail => gle(tail)
    
    gle(list).head

  /** Problem 2: Returns the last-but-one (penultimate) element of a list. Errors if list size less than 2 */
  def getLastButOneElement[T](list: List[T]): T =
    @tailrec
    def glbo(list: List[T]): List[T] = list match
      case Nil => throw NoSuchElementException("last-but-one of empty list")
      case head :: Nil => throw NoSuchElementException("last-but-one of list of size 1")
      case h1 :: h2 :: Nil => h1 :: Nil
      case head :: tail => glbo(tail)

    glbo(list).head

  /** Problem 3: Returns the kth element of a list. Errors if list size less than k or k less than 0 */
  def getKthElement[T](list: List[T], k: Int): T =
    val s = list.size
    if k >= s then throw NoSuchElementException(s"element $k of list of size $s")
    if k < 0 then throw IndexOutOfBoundsException(s"no such index $k")

    @tailrec
    def gke(list: List[T], acc: Int): List[T] = list match
      case Nil => throw NoSuchElementException("kth element of empty list")
      case head :: tail if acc == k => head :: Nil
      case head :: tail => gke(tail, acc + 1)

    gke(list, 0).head

  /** Problem 4: Returns the number of elements in a list. Returns 0 if list is empty */
  def listSize[T](list: List[T]): Int =
    @tailrec
    def ls(list: List[T], acc: Int): Int = list match
      case Nil => acc
      case head :: tail => ls(tail, acc + 1)
    ls(list, 0)

  /** Problem 5: Reverses a list */
  def reverseList[T](list: List[T]): List[T] = 
    @tailrec
    def rl(list: List[T], acc: List[T]): List[T] = list match
      case Nil => acc
      case head :: tail => rl(tail, head :: acc)
    rl(list, Nil)

  /** Problem 6: Return true if a list is a palindrome, false otherwise */
  def listIsPalindrome[T](list: List[T]): Boolean = list.size match
    case 0 => true
    // don't care about middle element for odd-length lists
    case s => list.take(s / 2) == reverseList(list.takeRight(s / 2))

  /** Problem 7: Returns a flattened list */
  def flattenList(list: List[Any]): List[Any] =
    def fl(list: List[Any]): List[Any] = 
      list.foldLeft(List.empty[Any]) { (acc, element) => element match
        case Nil          => acc
        case head :: tail => fl(head :: tail) ++: acc
        case item         => item :: acc
      }
    reverseList(fl(list))

  /** Problem 16: Drops every [n]th element in a list */
  def dropNthElement[T](n: Int, list: List[T]): List[T] = 
    if n <= 0 then return list // doesn't make sense for non-positive integers
    @tailrec
    def dne(c: Int, list: List[T], acc: List[T]): List[T] = list match
      case Nil => acc
      case head :: tail if c % n == 0 => dne(c + 1, tail, acc) 
      case head :: tail => dne(c + 1, tail, head :: acc)
    reverseList(dne(1, list, Nil))

  /** Problem 17: Splits a list into two parts by length */
  def splitByLength[T](len: Int, list: List[T]): (List[T], List[T]) = 
    if len <= 0 then return (list, Nil)
    @tailrec
    def sbl(c: Int, list: List[T], acc: List[T]): (List[T], List[T]) = list match
      case Nil => (reverseList(acc), Nil) // either len > list.size or list == Nil
      case head :: tail if c < len => sbl(c + 1, tail, head :: acc) // take
      case head :: tail => (reverseList(acc), head :: tail)
    sbl(0, list, Nil)

  /** Problem 18: Extracts a slice from a list */
  def getSlice[T](i: Int, j: Int, list: List[T]): List[T] =
    // if i == j then return Nil
    val (lower, higher) = if i < j then (i, j) else (j, i) // agnostic splice order
    @tailrec
    def gs(c: Int, list: List[T], acc: List[T]): List[T] = list match
      case Nil => acc
      case head :: tail if c >= lower && c < higher => gs(c + 1, tail, head :: acc)
      case l if c >= higher => acc // end early
      case head :: tail => gs(c + 1, tail, acc)
    reverseList(gs(0, list, Nil))

  /** Problem 19: Rotates a list [n] places to the left */
  def rotateLeft[T](n: Int, list: List[T]): List[T] =
    // if n == list.size then return list
    if n >= 0 then
      val (first, last) = splitByLength(n, list)
      last ++: first
    else
      val (first, last) = splitByLength(list.size + n, list) // add because n is negative
      last ++: first

  /** Problem 20: Removes the [k]th element from a list */
  def removeKthElement[T](k: Int, list: List[T]): (List[T], Option[T]) =
    @tailrec
    def rke(c: Int, list: List[T], acc: List[T]): (List[T], Option[T]) = list match
      case Nil => (reverseList(acc), None)
      case head :: tail if c == k => (reverseList(acc) ++: tail, Some(head)) // exit early if match
      case head :: tail => rke(c + 1, tail, head :: acc)
    rke(0, list, Nil)

  /** Problem 21: Inserts an element at position [k] in a list */
  def insertAt[T](k: Int, elem: T, list: List[T]): List[T] = 
    @tailrec
    def ia(c: Int, list: List[T], acc: List[T]): List[T] = list match
      case Nil if c == k => elem :: acc // insert at end
      case Nil => acc
      case head :: tail if c == k => ia(c + 1, tail, head :: elem :: acc)
      case head :: tail => ia(c + 1, tail, head :: acc)
    reverseList(ia(0, list, Nil))

  /** Problem 22: Creates a range of integers */
  def listRange(bound1: Int, bound2: Int, step: Int = 1): List[Int] =
    // check for unreachable second bound
    if (bound1 < bound2 && step <= 0) ||
       (bound2 < bound1 && step >= 0) then List(bound1)
    val between = (a: Int, b: Int, c: Int) => (a <= b && b <= c) || (c <= b && b <= a)
    @tailrec
    def lr(c: Int, acc: List[Int]): List[Int] = c match
      case n if between(bound1, n, bound2) => lr(c + step, n :: acc)
      case _ => acc
    reverseList(lr(bound1, Nil))

