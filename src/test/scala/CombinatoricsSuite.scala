object CombinatoricsSuite extends UnitSpec:
  import TestValues.*
  import ninety_nine.scala.problems.Combinatorics.*

  /** getCombinations */
  "getCombinations" should "get each unique combination of k objects in a list" in {
    assertResult(combinations)(getCombinations(2, List(1,2,3)))
  }
  it should "run efficiently for relatively large values of k and n" in {
    getCombinations(50, (for i <- 1 to 100 yield i).toList)
  }
  it should "return a list with an empty list on an empty list with k == 0" in {
    assertResult(Nil :: Nil)(getCombinations(0, Nil))
  }
  it should "return an empty list on an empty list with k != 0" in {
    assertResult(Nil)(getCombinations(1, Nil))
    assertResult(Nil)(getCombinations(-1, Nil))
  }
