package ninety_nine.scala.problems

import scala.annotation.tailrec
import scala.reflect.classTag
import java.util.NoSuchElementException
import java.lang.IndexOutOfBoundsException
import scala.reflect.ClassTag

/** Holds solutions to [99 Scala problems](https://aperiodic.net/pip/scala/s-99/#p01) */
object Solution:
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
