// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
import ninety_nine.scala.problems.Solution.*
import scala.reflect.ClassTag

class SolutionSuite extends munit.FunSuite {
  def checkEquals(tests: List[(String, Any, Any)])(using munit.Location): Unit =
    for (name, obtained, expected) <- tests do
      test(name)(assertEquals(obtained, expected))

  def checkFailsWithMessage[T <: Throwable : ClassTag](name: String, message: String)(body: => Any): Unit =
    test(name)(interceptMessage[T](message)(body))

  /** getLastElement */
  val getLastElementTests: List[(String, Any, Any)] = List(
    ("gets last element of string list", getLastElement(List("A", "B", "C")), "C"),
    ("gets last element of integer list", getLastElement(List(1,2,3,4,5)), 5),
    ("gets last element of one-element list", getLastElement(List(1)), 1),
    ("gets last element of long list", getLastElement((for i <- 1 to 10000 yield i).toList), 10000),
  )
  checkEquals(getLastElementTests)
  checkFailsWithMessage[NoSuchElementException](
    "fails to get last element of list", "last of empty list")(getLastElement(Nil))

  /** getLastButOneElement */
  val getLastButOneElementTests = List(
    ("gets last-but-one element of string list", getLastButOneElement(List("A", "B", "C")), "B"),
    ("gets last-but-one element of int list", getLastButOneElement(List(1,2,3,4)), 3),
    ("gets last-but-one of two-element list", getLastButOneElement(List(1,2)), 1),
    ("gets last-but-one element of long list", getLastButOneElement((for i <- 1 to 10000 yield i).toList), 9999),
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
    ("gets kth element of string list", getKthElement(List("A", "B", "C"), 1), "B"),
    ("gets kth element of int list", getKthElement(List(1,2,3,4,5), 3), 4),
    ("gets kth element of k-element list", getKthElement(List(1 -> 2, 3 -> 4), 1), 3 -> 4),
    ("gets kth element of long list", getKthElement((for i <- 1 to 10000 yield i).toList, 5000), 5001),
  )  
  checkEquals(getKthElementTests)
  checkFailsWithMessage[NoSuchElementException](
    "fails to get kth element of list with length less than k", "element 15 of list of size 3")
    (getKthElement(List(1,2,3), 15))
  checkFailsWithMessage[IndexOutOfBoundsException]
    ("fails to get kth element when k is less than zero", "no such index -1")
    (getKthElement(List("A", "B", "C"), -1))
}
