package spray.json.streaming

import org.specs2.mutable._

import java.io.StringWriter

class ProductStreamFormatsSpec extends Specification {
  import DefaultStreamProtocol._

  case class Test0()
  object Test0 {
    implicit val test0Format = jsonStreamFormat0(Test0.apply)
  }
  case class Test2(a: Int, b: Option[Double])
  object Test2 {
    implicit val test2Format = jsonStreamFormat2(Test2.apply)
  }
  case class Test5(a: Int, b: Double, c: String, d: String, e: Int)
  object Test5 {
    implicit val test5Format = jsonStreamFormat5(Test5.apply)
  }
  case class Nested(a: Test0, b: Test2)
  object Nested {
    implicit val testNestedFormat = jsonStreamFormat2(Nested.apply)
  }

  "A JsonFormat created with 'jsonStreamFormat0'" should {
    "write an object" in {
      val sw = new StringWriter
      PrettyStreamPrinter.printTo(sw, Test0())
      sw.toString === "{\n\n}"
    }
    "read an object" in {
      PullParser.read[Test0]("{}") === Test0()
    }
  }

  "A JsonFormat created with 'jsonStreamFormat2'" should {
    "write a value with Some" in {
      val sw = new StringWriter
      PrettyStreamPrinter.printTo(sw, Test2(1, Some(1.2)))
      sw.toString ===
        """{
        |  "a": 1,
        |  "b": 1.2
        |}""".stripMargin
    }
    "write a value with None" in {
      val sw = new StringWriter
      PrettyStreamPrinter.printTo(sw, Test2(22, None))
      sw.toString ===
        """{
        |  "a": 22
        |}""".stripMargin
    }
    "read a value with Some" in {
      PullParser.read[Test2]("""{ "b": 2.2, "a": 2}""") === Test2(2, Some(2.2))
    }
    "read a value with None" in {
      PullParser.read[Test2]("""{"a": 2}""") === Test2(2, None)
    }
  }

  "A JsonFormat created with 'jsonStreamFormat5'" should {
    "write a value" in {
      val sw = new StringWriter
      PrettyStreamPrinter.printTo(sw, Test5(33, 3.3, "foo", "bar", 44))
      sw.toString ===
        """{
        |  "a": 33,
        |  "b": 3.3,
        |  "c": "foo",
        |  "d": "bar",
        |  "e": 44
        |}""".stripMargin
    }
    "read a value" in {
      PullParser.read[Test5]("""{"b": 3.3,"e":44,"a":33,"d":"bar","c":"foo"}""") ===
        Test5(33, 3.3, "foo", "bar", 44)
    }
  }

  "a nested case class" should {
    "write a value" in {
      val sw = new StringWriter
      PrettyStreamPrinter.printTo(sw, Nested(Test0(), Test2(12, Some(3.4))))
      sw.toString ===
        """{
        |  "a": {
        |
        |  },
        |  "b": {
        |    "a": 12,
        |    "b": 3.4
        |  }
        |}""".stripMargin
    }
    "read a value" in {
      PullParser.read[Nested]("""{"b":{"a":12,"b":3.4},"a":{}}""") ===
        Nested(Test0(), Test2(12, Some(3.4)))
    }
  }
}
