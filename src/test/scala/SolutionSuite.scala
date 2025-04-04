// solutions
import ninety_nine.scala.problems.{ListOps,Duplicates,RunLengthEncoding}
// testing
import org.scalatest.*
import flatspec.*
import matchers.*

abstract class UnitSpec extends AnyFlatSpec with should.Matchers with OptionValues with Inside with Inspectors

class ListOpsSpec extends UnitSpec {
  // get test wrappers
  import ListOps.*

  // useful vals
  val largeNumber = 1000000
  val intList   = List(1,2,3,4)
  val largeList  = (for i <- 1 to largeNumber yield i).toList

  /** getLastElement */
  "getLastElement" should "get the last element of a list" in {
    assertResult(4)(getLastElement(intList))
  }
  it should "get the element of a list of size 1" in {
    assertResult(0)(getLastElement(List(0)))
  }
  it should "get the last element of a long list" in {
    assertResult(largeNumber)(getLastElement(largeList))
  }
  it should "throw an exception on an empty list" in {
    assertThrows[NoSuchElementException](getLastElement(Nil))
  }

  /** getLastButOneElement */
  "getLastButOneElement" should "get the last-but-one element of a list" in {
    assertResult(3)(getLastButOneElement(intList))
  }
  it should "get the last-but-one element of a large list" in {
    assertResult(largeNumber - 1)(getLastButOneElement(largeList))
  }
  it should "throw an exception on a one-element list" in {
    assertThrows[NoSuchElementException](getLastButOneElement(List(0)))
  }
  it should "throw an exception on an empty list" in {
    assertThrows[NoSuchElementException](getLastButOneElement(Nil))
  }

  /** getLastButOneElement */
  "getKthElement" should "get the kth element of a list" in {
    assertResult(3)(getKthElement(intList, 2))
  }
  it should "get the kth element of a list of size k + 1" in {
    assertResult(4)(getKthElement(intList, 3))
  }
  it should "get the kth element of a large list" in {
    assertResult(largeNumber / 2 + 1)(getKthElement(largeList, largeNumber / 2))
  }
  it should "throw an exception on a list of size less than k" in {
    assertThrows[NoSuchElementException](getKthElement(intList, 15))
  }
  it should "throw an exception when k is not a valid list index" in {
    assertThrows[IndexOutOfBoundsException](getKthElement(intList, -1))
  }

  /** listSize */
  "listSize" should "get size of a list" in {
    assertResult(4)(listSize(intList))
  }
  it should "get the size of an empty list" in {
    assertResult(0)(listSize(Nil))
  }
  it should "get the size of a large list" in {
    assertResult(largeNumber)(listSize(largeList))
  }

  /** reverseList */
  "reverseList" should "get the reverse of a list" in {
    assertResult(intList.reverse)(reverseList(intList))
  }
  it should "get the reverse of an empty list" in {
    assertResult(Nil)(reverseList(Nil))
  }
  it should "get the reverse of a large list" in {
    assertResult(largeList.reverse)(reverseList(largeList))
  }

  /** listIsPalindrome */
  val largePalindromicList = ((for i <- 1 to largeNumber / 2 yield i) ++ (for i <- largeNumber / 2 to 1 by -1 yield i)).toList
  "listIsPalindrome" should "return true for an even-sized palindromic list" in {
    assert(listIsPalindrome(List(1,2,2,1)))
  }
  it should "return false for an even-sized non-palindromic list" in {
    assert(!listIsPalindrome(intList))
  }
  it should "return true for an odd-sized palindromic list" in {
    assert(listIsPalindrome(List(1,2,3,2,1)))
  }
  it should "return false for an odd-sized non-palindromic list" in {
    assert(!listIsPalindrome(List(1,2,3,4,5)))
  }
  it should "return true for a large palindromic list" in {
    assert(listIsPalindrome(largePalindromicList))
  }
  it should "return false for a large non-palindromic list" in {
    assert(!listIsPalindrome(largeList))
  }
  it should "return true for a list of size 1" in {
    assert(listIsPalindrome(List(0)))
  }
  it should "return true for an empty list" in {
    assert(listIsPalindrome(Nil))
  }

  /** flattenList */
  val nestedIntList = List(List(1,1), 2, List(3, List(5,8)))
  val flattenedIntList = List(1,1,2,3,5,8)
  "flattenList" should "return a flattened list from a nested list" in {
    assertResult(flattenedIntList)(flattenList(nestedIntList))
  }
  it should "return from an empty list" in {
    assertResult(Nil)(flattenList(Nil))
  }
}

class DuplicatesSpec extends UnitSpec {  
  import Duplicates.*
  // useful vals
  val dupesStrList = List("a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e")
  /** removeConsecutiveDuplicates */
  val dedupedStrList = List("a", "b", "c", "a", "d", "e")
  "removeConsecutiveDuplicates" should "remove consecutive duplicates from a list" in {
    assertResult(dedupedStrList)(removeConsecutiveDuplicates(dupesStrList))
  }
  it should "return from an empty list" in {
    assertResult(Nil)(removeConsecutiveDuplicates(Nil))
  }

  /** packConsecutiveDuplicates */
  val packedStrList = 
    List(List("a", "a", "a", "a"), List("b"), List("c", "c"), 
    List("a", "a"), List("d"), List("e", "e", "e", "e"))
  "packConsecutiveDuplicates" should "return a packed list" in {
    println(packConsecutiveDuplicates(dupesStrList))
    assertResult(packedStrList)(packConsecutiveDuplicates(dupesStrList))
  }
  it should "return from an empty list" in {
    assertResult(Nil)(packConsecutiveDuplicates(Nil))
  }
}

class RunLengthEncodingSpec extends UnitSpec {
  import RunLengthEncoding.*
  val dupesStrList = List("a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e")
  /** encodeRunLength */
  val runLengthEncodedList = List((4,"a"), (1,"b"), (2,"c"), (2,"a"), (1,"d"), (4, "e"))
  "encodeRunLength" should "encode a list using run-length encoding" in {
    assertResult(runLengthEncodedList)(encodeRunLength(dupesStrList))
  }
  it should "return on an empty list" in {
    assertResult(Nil)(encodeRunLength(Nil))
  }

  /** encodeModifiedRunLength */
  val modifiedRunLengthEncodedList = List((4, "a"), "b", (2, "c"), (2, "a"), "d", (4, "e"))
  "encodeModifiedRunLength" should "encode a list using modified run-length encoding" in {
    assertResult(modifiedRunLengthEncodedList)(encodeModifiedRunLength(dupesStrList)) 
  }
  it should "return on an empty list" in {
    assertResult(Nil)(encodeModifiedRunLength(Nil))
  }

  /** decodeEncodedList */
  "decodeEncodedList" should "decode a list encoded with run-length encoding" in {
    assertResult(dupesStrList)(decodeEncodedList(runLengthEncodedList))
  }
  it should "return on an empty list" in {
    assertResult(Nil)(decodeEncodedList(Nil))
  }

  /** directEncodeRunLength */
  "directEncodeRunLength" should "encode a list using run-length encoding" in {
    assertResult(runLengthEncodedList)(directEncodeRunLength(dupesStrList))
  }
  it should "return on an empty list" in {
    assertResult(Nil)(directEncodeRunLength(Nil))
  }
}
