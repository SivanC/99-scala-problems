import ninety_nine.scala.problems.Duplicates.*
import TestValues.*

class DuplicatesSpec extends UnitSpec {  
  /** removeConsecutiveDuplicates */
  "removeConsecutiveDuplicates" should "remove consecutive duplicates from a list" in {
    assertResult(dedupedStrList)(removeConsecutiveDuplicates(dupesStrList))
  }
  it should "return from an empty list" in {
    assertResult(Nil)(removeConsecutiveDuplicates(Nil))
  }

  /** packConsecutiveDuplicates */
  "packConsecutiveDuplicates" should "return a packed list" in {
    assertResult(packedStrList)(packConsecutiveDuplicates(dupesStrList))
  }
  it should "return from an empty list" in {
    assertResult(Nil)(packConsecutiveDuplicates(Nil))
  }

  /** duplicateElementsOnce */
  "duplicateElementsOnce" should "duplicate each element of a list once, in place" in {
    assertResult(duplicatedOnceList)(duplicateElementsOnce(intList))
  }
  it should "return on an empty list" in {
    assertResult(Nil)(duplicateElementsOnce(Nil))
  }

  /** duplicateElements */
  "duplicateElements" should "duplicate the elements of a list a certain number of times" in {
    assertResult(duplicatedTwiceList)(duplicateElements(3, intList))
  }
  it should "return the original list if times is less than 2" in {
    assertResult(intList)(duplicateElements(-1, intList))
  }
}

