// testing
import org.scalatest.*
import flatspec.*
import matchers.*

abstract class UnitSpec extends AnyFlatSpec with should.Matchers with OptionValues with Inside with Inspectors

object TestValues:
  /** General */
  val intList   = List(1,2,3,4)

  /** ListOps */
  val largeNumber = 1000000
  val largeList  = (for i <- 1 to largeNumber yield i).toList
  val largePalindromicList = ((for i <- 1 to largeNumber / 2 yield i) ++ (for i <- largeNumber / 2 to 1 by -1 yield i)).toList
  val nestedIntList = List(List(1,1), 2, List(3, List(5,8)))
  val flattenedIntList = List(1,1,2,3,5,8)
  val everySecondDropped = List(1,3)
  val splitList = (List(1,2), List(3,4))
  val intListAndNil = (intList, Nil)
  val twiceNil = (Nil, Nil)
  val intSlice = List(2,3)
  val rotatedIntList = List(3, 4, 1, 2)
  val removeThirdIntList = List(1,2,4)
  val insertedIntList = List(1, 2, 5, 3, 4)

  /** Duplicates */
  val dupesStrList = List("a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e")
  val dedupedStrList = List("a", "b", "c", "a", "d", "e")
  val packedStrList = 
    List(List("a", "a", "a", "a"), List("b"), List("c", "c"), 
    List("a", "a"), List("d"), List("e", "e", "e", "e"))
  val duplicatedOnceList = List(1,1,2,2,3,3,4,4)
  val duplicatedTwiceList = List(1,1,1,2,2,2,3,3,3,4,4,4)

  /** RunLengthEncoding */
  val runLengthEncodedList = List((4,"a"), (1,"b"), (2,"c"), (2,"a"), (1,"d"), (4, "e"))
  val modifiedRunLengthEncodedList = List((4, "a"), "b", (2, "c"), (2, "a"), "d", (4, "e"))
  
  /** ListRandom */
  val randIntList = List(2,1,4,3)
