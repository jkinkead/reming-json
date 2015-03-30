package spray.json.streaming

import spray.json.DeserializationException

import org.specs2.mutable._

class PullParserSpec extends Specification {
  "readBoolean()" should {
    "parse 'false'" in {
      PullParser.withInput("false").readBoolean() === false
    }
    "parse 'true'" in {
      PullParser.withInput("true").readBoolean() === true
    }
    "fail on other input" in {
      PullParser.withInput("[]").readBoolean() must throwA[DeserializationException]
    }
  }

  "readString()" should {
    """parse '"foo"'""" in {
      PullParser.withInput(""""foo"""").readString() === "foo"
    }
    "fail on other input" in {
      PullParser.withInput("false").readString() must throwA[DeserializationException]
    }
  }

  "manual array parsing" should {
    import DefaultStreamProtocol._
    "handle a single-element array" in {
      val parser = PullParser.withInput("[ 123]")
      parser.startArray[Int]() === 123
      parser.endArray()
      parser.cursorChar === '\uFFFF'
    }
    "handle a heterogeneous array" in {
      val parser = PullParser.withInput("""["abc" ,123 ]""")
      parser.startArray[String]() === "abc"
      parser.readArrayItem[Int]() === 123
      parser.endArray()
      parser.cursorChar === '\uFFFF'
    }
  }

  "iterator array parsing" should {
    import DefaultStreamProtocol._
    "handle an empty array" in {
      val parser = PullParser.withInput("[ ]")
      parser.readArray[Int]().toSeq === Seq()
      parser.cursorChar === '\uFFFF'
    }
    "handle an array" in {
      val parser = PullParser.withInput("[1, 2,3 ,4]")
      parser.readArray[Int]().toSeq === Seq(1, 2, 3, 4)
      parser.cursorChar === '\uFFFF'
    }
  }

  "object parsing" should {
    "handle an empty object" in {
      val parser = PullParser.withInput("{}")
      parser.startObject()
      parser.endObject()
      parser.cursorChar === '\uFFFF'
    }
    "handle an object with keys" in {
      val parser = PullParser.withInput("""{"a" : "string", "b" : false}""")
      import DefaultStreamProtocol._
      parser.startObject()
      val booleanValue = parser.readField[Boolean]("b")
      val stringValue = parser.readField[String]("a")
      parser.endObject()
      booleanValue.value === false
      stringValue.value === "string"
      parser.cursorChar === '\uFFFF'
    }
    "set default values for handlers" in {
      val parser = PullParser.withInput("""{"a" : "string"}""")
      import DefaultStreamProtocol._
      parser.startObject()
      val booleanValue = parser.readField[Boolean]("b")
      val stringValue = parser.readField[String]("a")
      parser.endObject()
      booleanValue.optionalValue === None
      stringValue.value === "string"
      parser.cursorChar === '\uFFFF'
    }
    "skip keys without handlers" in {
      val parser = PullParser.withInput("""{"a" : "string"}""")
      import DefaultStreamProtocol._
      parser.startObject()
      parser.endObject()
      parser.cursorChar === '\uFFFF'
    }
    "fail on unterminated objects" in {
      val parser = PullParser.withInput("""{"a" : "string" """)
      import DefaultStreamProtocol._
      parser.startObject()
      parser.endObject() must throwA[DeserializationException]
    }
    "fail on other input" in {
      PullParser.withInput("false").startObject() must throwA[DeserializationException]
    }
  }

  "implicit parsing with a custom format" should {
    case class MyType(name: String, hasMonkey: Boolean)

    import DefaultStreamProtocol._
    implicit val MyTypeReader = new JsonStreamReader[MyType] {
      def read(parser: PullParser): MyType = {
        parser.startObject()
        val name = parser.readField[String]("name")
        val hasMonkey = parser.readField[Boolean]("hasMonkey")
        parser.endObject()
        MyType(name.value, hasMonkey.value)
      }
    }

    "correctly deserialize" in {
      val instance: MyType = PullParser.read[MyType](
        """{ "name": "Paul McCartney", "hasMonkey": true }"""
      )
      instance mustEqual MyType("Paul McCartney", true)
    }
  }
}
