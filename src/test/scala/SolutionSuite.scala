// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
import ninety_nine.scala.problems.{ListOps,Duplicates,RunLengthEncoding}
import scala.reflect.ClassTag

import TestSuite.*

object TestSuite:
  def checkEquals(tests: List[(String, Any, Any)])(using munit.Location): Unit =
    for (name, obtained, expected) <- tests do
      test(name)(assertEquals(obtained, expected))

  def checkCondition(name: String)(body: => Boolean)(using munit.Location): Unit =
    test(name)(assert(body))

  def checkFailsWithMessage[T <: Throwable : ClassTag](name: String, message: String)(body: => Any)(using munit.Location): Unit =
    test(name)(interceptMessage[T](message)(body))

class ListOpsSuite extends munit.FunSuite {
  // get test wrappers
  import ListOps.*

  // useful vals
  val largeNumber = 1000000
  val intList   = List(1,2,3,4)
  val strList   = List("A", "B", "C", "D")
  val largeList  = (for i <- 1 to largeNumber yield i).toList

  /** getLastElement */
  val getLastElementTests: List[(String, Any, Any)] = List(
    ("gets last element of string list", getLastElement(strList), "D"),
    ("gets last element of integer list", getLastElement(intList), 4),
    ("gets last element of one-element list", getLastElement(List(1)), 1),
    ("gets last element of long list", getLastElement(largeList), largeNumber),
  )
  checkEquals(getLastElementTests)
  checkFailsWithMessage[NoSuchElementException](
    "fails to get last element of list", "last of empty list")(getLastElement(Nil))

  /** getLastButOneElement */
  val getLastButOneElementTests = List(
    ("gets last-but-one element of string list", getLastButOneElement(strList), "C"),
    ("gets last-but-one element of int list", getLastButOneElement(intList), 3),
    ("gets last-but-one of two-element list", getLastButOneElement(List(1,2)), 1),
    ("gets last-but-one element of long list", getLastButOneElement(largeList), largeNumber - 1),
  )
  checkEquals(getLastButOneElementTests)
  checkFailsWithMessage[NoSuchElementException](
    "fails to get last-but-one element of one-element list", "last-but-one of list of size 1")
    (getLastButOneElement(List(1)))
  checkFailsWithMessage[NoSuchElementException](
    "fails to get last-but-one element of empty list", "last-but-one of empty list")
    (getLastButOneElement(Nil))

  /** getLastButOneElement */
  val getKthElementTests = List(
    ("gets kth element of string list", getKthElement(strList, 1), "B"),
    ("gets kth element of int list", getKthElement(intList, 2), 3),
    ("gets kth element of k-element list", getKthElement(List(1 -> 2, 3 -> 4), 1), 3 -> 4),
    ("gets kth element of long list", 
      getKthElement(largeList, largeNumber / 2), largeNumber / 2 + 1),
  )  
  checkEquals(getKthElementTests)
  checkFailsWithMessage[NoSuchElementException](
    "fails to get kth element of list with length less than k", "element 15 of list of size 4")
    (getKthElement(intList, 15))
  checkFailsWithMessage[IndexOutOfBoundsException]
    ("fails to get kth element when k is less than zero", "no such index -1")
    (getKthElement(intList, -1))

  /** listSize */
  val listSizeTests = List(
    ("gets size of int list", listSize(intList), 4),
    ("gets size of empty list", listSize(Nil), 0),
    ("gets size of large list", listSize(largeList), largeNumber),
  )
  checkEquals(listSizeTests)

  /** reverseList */
  val reverseListTests = List(
    ("gets reverse of list of ints", reverseList(intList), List(4,3,2,1)),
    ("gets reverse of empty list", reverseList(Nil), Nil),
    ("gets reverse of long list", 
      reverseList(largeList), (for i <- largeNumber to 1 by -1 yield i).toList)
  )
  checkEquals(reverseListTests)

  /** listIsPalindrome */
  val largePalindromicList = ((for i <- 1 to largeNumber / 2 yield i) ++ (for i <- largeNumber / 2 to 1 by -1 yield i)).toList
  val listIsPalindromeTests = List(
    ("returns true for even-size palindromic list", listIsPalindrome(List(1,2,2,1))),
    ("returns false for even-size non-palindromic list", !listIsPalindrome(intList)),
    ("returns true for odd-size palindromic list", listIsPalindrome(List(1,2,3,2,1))),
    ("returns false for odd-size non-palindromic list", !listIsPalindrome(List(1,2,3,4,5))),
    ("returns true for large palindromic list", listIsPalindrome(largePalindromicList)),
    ("returns true for list of size 1", listIsPalindrome(List(1))),
    ("returns true for empty list", listIsPalindrome(Nil)),
  )
  for (name, body) <- listIsPalindromeTests do checkCondition(name)(body)

  /** flattenList */
  val nestedIntList = List(List(1,1), 2, List(3, List(5,8)))
  val flattenedIntList = List(1,1,2,3,5,8)
  val flattenListTests = List(
    ("returns a flattened list of ints", flattenList(nestedIntList), flattenedIntList),
    ("returns a flattened empty list", flattenList(List(Nil)), Nil)
  )
  checkEquals(flattenListTests)
}

class DuplicatesSuite extends munit.FunSuite {  
  import Duplicates.*
  // useful vals
  val dupesStrList = List("a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e")
  val dupesIntList = List(9, 8, 9, 9, 3, 3, 4, 1, 6, 6)
  /** removeConsecutiveDuplicates */
  val dedupedStrList = List("a", "b", "c", "a", "d", "e")
  val dedupedIntList = List(9, 8, 9, 3, 4, 1, 6)
  val removeConsecutiveDuplicatesTests = List(
    ("returns a list of strs without cons. dupes", removeConsecutiveDuplicates(dupesStrList), dedupedStrList),
    ("returns a list of ints without cons. dupes", removeConsecutiveDuplicates(dupesIntList), dedupedIntList),
    ("removeConsecutiveDuplicates works on empty list", removeConsecutiveDuplicates(Nil), Nil),
  )
  checkEquals(removeConsecutiveDuplicatesTests)

  /** packConsecutiveDuplicates */
  val packedStrList = 
    List(List("a", "a", "a", "a"), List("b"), List("c", "c"), 
    List("a", "a"), List("d"), List("e", "e", "e", "e"))
  val packedIntList = List(List(9), List(8), List(9, 9), List(3, 3), List(4), List(1), List(6, 6))
  val packConsecutiveDuplicatesTests = List(
    ("returns a packed str list", packConsecutiveDuplicates(dupesStrList), packedStrList),
    ("returns a packed int list", packConsecutiveDuplicates(dupesIntList), packedIntList),
    ("returns a packed empty list", packConsecutiveDuplicates(Nil), Nil)
  )
  checkEquals(packConsecutiveDuplicatesTests)
}
