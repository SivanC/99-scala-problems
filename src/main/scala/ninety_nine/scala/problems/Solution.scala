package ninety_nine.scala.problems

import scala.annotation.tailrec
import scala.reflect.classTag
import java.util.NoSuchElementException
import java.lang.IndexOutOfBoundsException
import scala.reflect.ClassTag

/** Solutions related to abstract manipulation of lists */
object ListOps:
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

/** Solutions related to duplicate elements in lists */
object Duplicates:
  import ListOps.*
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

object RunLengthEncoding:
  import ListOps.*
  import Duplicates.*
  /** Problem 10: Uses packConsecutiveDuplicates to implement run-length
   *  encoding. Consecutive duplicates of elements are encoded as tuples (N, E)
   *  where N is the number of duplicates of the element E. 
   */
  def encodeRunLength[T](list: List[T]): List[(Int, T)] =
    val packedList: List[List[T]] = packConsecutiveDuplicates(list)
    packedList.map{ elem => 
      (listSize(elem), getKthElement(elem, 0))
    }

  /** Problem 11: Uses packConsecutiveDuplicates to implement run-length
   *  encoding. Differs from [[encodeRunLength]] in that runs of length 1 are
   *  encoded directly and not as a list
   */
  def encodeModifiedRunLength[T](list: List[T]): List[(Int, T) | T] =
    val packedList: List[List[T]] = packConsecutiveDuplicates(list)
    packedList.map{ elem => listSize(elem) match
      case 1 => elem.head
      case _ => (listSize(elem), getKthElement(elem, 0))
    }

  /** Problem 12: Decodes a run-length encoded list that encodes single-length
   *  runs in lists.
   */
  def decodeEncodedList[T](list: List[(Int, T)]): List[T] =
    @tailrec
    def dl(list: List[(Int, T)], acc: List[T]): List[T] = list match
      case Nil          => acc
      case head :: tail => dl(tail,
        (for i <- 0 until head.head yield head.last).toList ++: acc) // build decoded run
    reverseList(dl(list, Nil))

  /** Problem 13: Encodes a list with run-length encoding directly (no helper
   *  methods from the package)
   */
  def directEncodeRunLength[T](list: List[T]): List[(Int, T)] = 
    if list.size == 0 then return Nil // guard so we can pass "default" value for current run element of type T
    @tailrec
    def derl(list: List[T], run: (Int, T), acc: List[(Int, T)]): List[(Int, T)] = list match
      case Nil => run :: acc // end
      case head :: tail if run.head == 0 || run.last == head => // first run or continue
        derl(tail, (run.head + 1, head), acc)
      case head :: tail => 
        derl(tail, (1, head), run :: acc) // new non-first run
    reverseList(derl(list, (0, list.head), Nil)) // use head of list as filler value since it has to be of type T
