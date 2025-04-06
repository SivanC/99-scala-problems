import ninety_nine.scala.problems.RunLengthEncoding.*
import TestValues.*

class RunLengthEncodingSpec extends UnitSpec {
  /** encodeRunLength */
  "encodeRunLength" should "encode a list using run-length encoding" in {
    assertResult(runLengthEncodedList)(encodeRunLength(dupesStrList))
  }
  it should "return on an empty list" in {
    assertResult(Nil)(encodeRunLength(Nil))
  }

  /** encodeModifiedRunLength */
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
