/*
 * Copyright (C) 2015 by Jesse Kinkead
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package reming

import org.specs2.mutable._

class JsonParserSpec extends Specification {
  "readBoolean()" should {
    "parse 'false'" in {
      JsonParser.withString("false").readBoolean() === false
    }
    "parse 'true'" in {
      JsonParser.withString("true").readBoolean() === true
    }
    "fail on other input" in {
      JsonParser.withString("[]").readBoolean() must throwA[DeserializationException]
    }
  }

  "readString()" should {
    """parse '"foo"'""" in {
      JsonParser.withString(""""foo"""").readString() === "foo"
    }
    "fail on other input" in {
      JsonParser.withString("false").readString() must throwA[DeserializationException]
    }
  }

  "manual array parsing" should {
    import DefaultProtocol._
    "handle a single-element array" in {
      val parser = JsonParser.withString("[ 123]")
      parser.startArray[Int]() === 123
      parser.endArray()
      parser.cursorChar === '\uFFFF'
    }
    "handle a heterogeneous array" in {
      val parser = JsonParser.withString("""["abc" ,123 ]""")
      parser.startArray[String]() === "abc"
      parser.readArrayItem[Int]() === 123
      parser.endArray()
      parser.cursorChar === '\uFFFF'
    }
  }

  "iterator array parsing" should {
    import DefaultProtocol._
    "handle an empty array" in {
      val parser = JsonParser.withString("[ ]")
      parser.readArray[Int]().toSeq === Seq()
      parser.cursorChar === '\uFFFF'
    }
    "handle an array" in {
      val parser = JsonParser.withString("[1, 2,3 ,4]")
      parser.readArray[Int]().toSeq === Seq(1, 2, 3, 4)
      parser.cursorChar === '\uFFFF'
    }
  }

  "object parsing" should {
    "handle an empty object" in {
      val parser = JsonParser.withString("{}")
      parser.startObject()
      parser.endObject()
      parser.cursorChar === '\uFFFF'
    }
    "handle an object with keys" in {
      val parser = JsonParser.withString("""{"a" : "string", "b" : false}""")
      import DefaultProtocol._
      parser.startObject()
      val booleanValue = parser.readField[Boolean]("b")
      val stringValue = parser.readField[String]("a")
      parser.endObject()
      booleanValue.value === false
      stringValue.value === "string"
      parser.cursorChar === '\uFFFF'
    }
    "set default values for handlers" in {
      val parser = JsonParser.withString("""{"a" : "string"}""")
      import DefaultProtocol._
      parser.startObject()
      val booleanValue = parser.readField[Boolean]("b")
      val stringValue = parser.readField[String]("a")
      parser.endObject()
      booleanValue.optionalValue === None
      stringValue.value === "string"
      parser.cursorChar === '\uFFFF'
    }
    "skip keys without handlers" in {
      val parser = JsonParser.withString("""{"a": "string",
        "b": [ "array", "of", [ "stuff" ]],
        "c": { "nested": "object to skip" }
      }""")
      import DefaultProtocol._
      parser.startObject()
      parser.endObject()
      parser.cursorChar === '\uFFFF'
    }
    "fail on unterminated objects" in {
      val parser = JsonParser.withString("""{"a" : "string" """)
      import DefaultProtocol._
      parser.startObject()
      parser.endObject() must throwA[DeserializationException]
    }
    "fail on other input" in {
      JsonParser.withString("false").startObject() must throwA[DeserializationException]
    }
  }

  "implicit parsing with a custom format" should {
    case class MyType(name: String, hasMonkey: Boolean)

    import DefaultProtocol._
    implicit val MyTypeReader = new JsonReader[MyType] {
      def read(parser: JsonParser): MyType = {
        parser.startObject()
        val name = parser.readField[String]("name")
        val hasMonkey = parser.readField[Boolean]("hasMonkey")
        parser.endObject()
        MyType(name.value, hasMonkey.value)
      }
    }

    "correctly deserialize" in {
      val instance: MyType = JsonParser.read[MyType](
        """{ "name": "Paul McCartney", "hasMonkey": true }"""
      )
      instance mustEqual MyType("Paul McCartney", true)
    }
  }
}
