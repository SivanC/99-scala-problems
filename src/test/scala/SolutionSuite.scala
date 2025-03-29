// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
import ninety_nine.scala.problems.Solution.*
import scala.reflect.ClassTag

class SolutionSuite extends munit.FunSuite {
  def checkEquals[T](name: String, obtained: T, expected: T)(using munit.Location): Unit =
    test(name) {
      assertEquals(obtained, expected)
    } 

  /** getLastElement */
  checkEquals("get last element of integer array", 
    getLastElement(List(1,2,3,4,5)), 
    5)
  checkEquals("get last element of string array",
    getLastElement(List("this", "is", "a", "test")),
    "test")
  test("fail to get last element of empty list") {
    interceptMessage[java.util.NoSuchElementException]("last of empty list") {
      getLastElement(Nil)
    }
  }
  checkEquals("get last element of very large list",
    getLastElement((for i <- 1 to 10000 yield i).toList),
    10000)
}
