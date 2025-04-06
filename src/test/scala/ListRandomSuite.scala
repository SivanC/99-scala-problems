import ninety_nine.scala.problems.ListRandom.{seededRng,*} // get given
import TestValues.*

class ListRandomSpec extends UnitSpec {
  import util.Using
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
