// solutions
import ninety_nine.scala.problems.{ListOps,Duplicates,RunLengthEncoding,ListRandom}
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

  /** dropNthElement */
  val everySecondDropped = List(1,3)
  "dropNthElement" should "drop every nth element of a list" in {
    assertResult(everySecondDropped)(dropNthElement(2, intList))
  }
  it should "return the original list if n > list size" in {
    assertResult(intList)(dropNthElement(100, intList))
  }
  it should "return the original list if n is non-positive" in {
    assertResult(intList)(dropNthElement(0, intList))
    assertResult(intList)(dropNthElement(-1, intList))
  }
  it should "return an empty list if n is 1" in {
    assertResult(Nil)(dropNthElement(1, intList))
  }
  it should "return on an empty list" in {
    assertResult(Nil)(dropNthElement(1, Nil))
  }

  /** splitByLength */
  val splitList = (List(1,2), List(3,4))
  val intListAndNil = (intList, Nil)
  val twiceNil = (Nil, Nil)
  "splitByLength" should "split a list into two parts, with the first of length len" in {
    assertResult(splitList)(splitByLength(2, intList))
  }
  it should "return the original list with Nil if len is >= length or non-positive" in {
    assertResult(intListAndNil)(splitByLength(100, intList))
    assertResult(intListAndNil)(splitByLength(0, intList))
    assertResult(intListAndNil)(splitByLength(-1, intList))
  }
  it should "return a pair of empty lists if the list is empty" in {
    assertResult(twiceNil)(splitByLength(1, Nil))
  }

  /** getSlice */
  val intSlice = List(2,3)
  "getSlice" should "return a specified slice of a list" in {
    assertResult(intSlice)(getSlice(1, 3, intList))
    assertResult(intSlice)(getSlice(3, 1, intList))
  }
  it should "return the original list when the slice parameters span the whole list" in {
    assertResult(intList)(getSlice(0, intList.size, intList))
  }
  it should "return Nil when slice parameters are equal, too large, or too small" in {
    assertResult(Nil)(getSlice(1, 1, intList))
    assertResult(Nil)(getSlice(-2, -1, intList))
    assertResult(Nil)(getSlice(8, 9, intList))
  }
  it should "return on an empty list" in {
    assertResult(Nil)(getSlice(3, 4, Nil))
    assertResult(Nil)(getSlice(0, 1, Nil))
  }

  /** rotateLeft */
  val rotatedIntList = List(3, 4, 1, 2)
  "rotateLeft" should "rotate a list left by n indices" in {
    assertResult(rotatedIntList)(rotateLeft(2, intList))
  }
  it should "rotate a list right if n is negative" in {
    assertResult(rotatedIntList)(rotateLeft(-2, intList))
  }
  it should "return the original list if n is 0 or list size" in {
    assertResult(intList)(rotateLeft(4, intList))
    assertResult(intList)(rotateLeft(0, intList))
  }
  it should "return on an empty list" in {
    assertResult(Nil)(rotateLeft(1, Nil))
  }

  /** removeKthElement */
  val removeThirdIntList = List(1,2,4)
  "removeKthElement" should "remove the kth element from a list" in {
    assertResult((removeThirdIntList, Some(3)))(removeKthElement(2, intList)) // zero-indexed
  }
  it should "return the original list if k is not a valid index" in {
    assertResult((intList, None))(removeKthElement(100, intList))
    assertResult((intList, None))(removeKthElement(-1, intList))
  }
  it should "return on an empty list" in {
    assertResult((Nil, None))(removeKthElement(1, Nil))
  }

  /** insertAt */
  val insertedIntList = List(1, 2, 5, 3, 4)
  "insertAt" should "insert an element at the given position in a list" in {
    assertResult(insertedIntList)(insertAt(2, 5, intList))
  }
  it should "insert an element at the beginning of a list" in {
    assertResult(List(5, 1, 2, 3, 4))(insertAt(0, 5, intList))
  }
  it should "insert an element at the end of a list" in {
    assertResult(List(1, 2, 3, 4, 5))(insertAt(intList.size, 5, intList))
  }
  it should "not insert an element in a list when k is an invalid index" in {
    assertResult(intList)(insertAt(15, 1, intList))
  }
  it should "insert an element in an empty list" in {
    assertResult(List(1))(insertAt(0, 1, Nil))
  }

  /** listRange */
  "listRange" should "create a range of numbers" in {
    assertResult(intList)(listRange(1, 4))
  }
  it should "use a custom step if provided" in {
    assertResult(List(1, 3))(listRange(1, 4, 2))
  }
  it should "generate the numbers based on the order of the bounds" in {
    assertResult(reverseList(intList))(listRange(4, 1, -1))
    assertResult(List(4, 2))(listRange(4, 1, -2))
  }
  it should "generate a single-element range if the bounds are equal" in {
    assertResult(List(1))(listRange(1, 1))
    assertResult(List(1))(listRange(1, 1, 5))
  }
  it should "return a list with the first bound if the second bound is unreachable" in {
    assertResult(List(4))(listRange(4, 1, 2))
    assertResult(List(1))(listRange(1, 4, -2))
  }
}

class DuplicatesSpec extends UnitSpec {  
  import Duplicates.*
  // useful vals
  val intList = List(1,2,3,4)
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
    assertResult(packedStrList)(packConsecutiveDuplicates(dupesStrList))
  }
  it should "return from an empty list" in {
    assertResult(Nil)(packConsecutiveDuplicates(Nil))
  }

  /** duplicateElementsOnce */
  val duplicatedOnceList = List(1,1,2,2,3,3,4,4)
  "duplicateElementsOnce" should "duplicate each element of a list once, in place" in {
    assertResult(duplicatedOnceList)(duplicateElementsOnce(intList))
  }
  it should "return on an empty list" in {
    assertResult(Nil)(duplicateElementsOnce(Nil))
  }

  /** duplicateElements */
  val duplicatedTwiceList = List(1,1,1,2,2,2,3,3,3,4,4,4)
  "duplicateElements" should "duplicate the elements of a list a certain number of times" in {
    assertResult(duplicatedTwiceList)(duplicateElements(3, intList))
  }
  it should "return the original list if times is less than 2" in {
    assertResult(intList)(duplicateElements(-1, intList))
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

class ListRandomSpec extends UnitSpec {
  import ListRandom.{seededRng, *} // get RNG too
  import util.Using

  val intList = List(1,2,3,4)
  val randIntList = List(2,1,4,3)
  /** extractRandomFromList */
  "extractRandomFromList" should "extract n random elements from a list" in {
    assertResult(randIntList)(extractRandomFromList(4, intList))
  }
  it should "not fail when n is higher than the size of the list" in {
    extractRandomFromList(15, randIntList)
  }
  it should "return on an empty list" in {
    assertResult(Nil)(extractRandomFromList(1, Nil))
  }

  /** drawNumbers */
  "drawNumbers" should "draw n different random numbers in the range 1 to m" in {
    val nums = drawNumbers(99, 99) // get all numbers in range to maximize chance of finding duplicates
    assert(nums.toSet.size == nums.size)
    assert(nums.size == 99)
  }
  it should "not fail when n is greater than m" in {
    val nums = drawNumbers(50, 10)
    assert(nums.toSet.size == nums.size)
    assert(nums.size == 10)
  }
  it should "draw numbers when m is less than 1" in {
    val nums = drawNumbers(5, -3)
    assert(nums.toSet.size == nums.size)
    assert(nums.size == 5)
  }

  /** listPermutation */
  "listPermutation" should "generate a random permutation of a list" in {
    val perm = listPermutation(intList)
    assert(perm.toSet == intList.toSet)
  }
  it should "return on an empty list" in {
    assertResult(Nil)(listPermutation(Nil))
  }
}
