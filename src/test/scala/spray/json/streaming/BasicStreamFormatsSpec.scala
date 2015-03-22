package spray.json.streaming

import org.specs2.mutable._

import java.io.StringWriter

class BasicStreamFormatsSpec extends Specification {
  import DefaultStreamProtocol._

  "Int format" should {
    "read an int" in {
      PullParser.read[Int]("123") === 123
    }
    "write an int" in {
      val sw = new StringWriter
      PrettyStreamPrinter.printTo(sw, 123)
      sw.toString === "123"
    }
  }

  "String format" should {
    "read a string" in {
      PullParser.read[String](""""abc"""") === "abc"
    }
    "write a string" in {
      val sw = new StringWriter
      PrettyStreamPrinter.printTo(sw, "abc")
      sw.toString === """"abc""""
    }
  }
}
