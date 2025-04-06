package ninety_nine.scala.problems

/** Solutions related to run-length encoding and decoding */
object RunLengthEncoding:
  import ListOps.*
  import Duplicates.*
  import scala.annotation.tailrec

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

