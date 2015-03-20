package spray.json.streaming

import spray.json.DeserializationException
import spray.json.JsonParser.ParsingException

import org.specs2.mutable._

class StandardStreamFormatsSpec extends Specification {
  import DefaultStreamProtocol._

  "Option format" should {
    "read None" in {
      PullParser.read[Option[String]]("null") === None
    }
    "read Some" in {
      PullParser.read[Option[String]](""""Exists"""") === Some("Exists")
    }
  }
  "Either format" should {
    "read Right" in {
      PullParser.read[Either[String, Int]]("123") === Right(123)
    }
    "read Left" in {
      PullParser.read[Either[String, Int]](""""str"""") === Left("str")
    }
  }

  "Tuple1 format" should {
    "read values" in {
      PullParser.read[Tuple1[Int]]("22") mustEqual Tuple1(22)
    }
  }
  "Tuple2 format" should {
    "read values" in {
      PullParser.read[(Int, Double)]("[22, 1.0]") mustEqual (22, 1.0)
    }
  }
  "Tuple3 format" should {
    "read values" in {
      PullParser.read[(Int, Double, String)]("""[22, 1.0, "str"]""") mustEqual (22, 1.0, "str")
    }
  }
  "Tuple4 format" should {
    "read values" in {
      PullParser.read[(Int, Double, String, Int)]("""[22, 1.0, "str", 42]""") mustEqual
        (22, 1.0, "str", 42)
    }
  }
  "Tuple5 format" should {
    "read values" in {
      PullParser.read[(Int, Double, String, Int, Int)]("""[22, 1.0, "str", 42, 41]""") mustEqual
        (22, 1.0, "str", 42, 41)
    }
  }
  "Tuple6 format" should {
    "read values" in {
      PullParser.read[(Int, Double, String, Int, Int, Int)](
        """[22, 1.0, "str", 42, 41, 40]"""
      ) mustEqual (22, 1.0, "str", 42, 41, 40)
    }
  }
  "Tuple7 format" should {
    "read values" in {
      PullParser.read[(Int, Double, String, Int, Int, Int, String)](
        """[22, 1.0, "str", 42, 41, 40, "i"]"""
      ) mustEqual (22, 1.0, "str", 42, 41, 40, "i")
    }
  }
}
