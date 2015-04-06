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

import java.io.StringWriter

class CollectionStreamFormatsSpec extends Specification with DefaultStreamProtocol {

  "listFormat" should {
    val list = List(1, 2, 3)
    "serialize a List[Int] to an array" in {
      val sw = new StringWriter
      PrettyStreamPrinter.printTo(sw, list)
      sw.toString === "[1, 2, 3]"
    }
    "read a serialized int array" in {
      PullParser.read[List[Int]]("[1,2,3]") === list
    }
  }

  "arrayFormat" should {
    val array = Array(1, 2, 3)
    "serialize an Array[Int] to an int array" in {
      val sw = new StringWriter
      PrettyStreamPrinter.printTo(sw, array)
      sw.toString === "[1, 2, 3]"
    }
    "read a serialized int array" in {
      PullParser.read[Array[Int]]("[1,2,3]") === array
    }
  }

  "mapFormat" should {
    val map = Map("a" -> 1, "b" -> 2, "c" -> 3)
    "serialize a Map[String, Long] to an object" in {
      val sw = new StringWriter
      PrettyStreamPrinter.printTo(sw, map)
      sw.toString ===
        """{
          |  "a": 1,
          |  "b": 2,
          |  "c": 3
          |}""".stripMargin
    }
    "read an object with long values to a Map[String, Long]" in {
      PullParser.read[Map[String, Long]]("""{"a":1,"c":3,"b":2}""") === map
    }
  }

  case class TestObject(a: String, b: Option[Int])
  object TestObject {
    implicit val innerFormat = jsonStreamFormat2(TestObject.apply)
  }
  "seq format" should {
    val seq = Seq(TestObject("a", None), TestObject("b", Some(1)))
    "serialize a seq of objects" in {
      val sw = new StringWriter
      PrettyStreamPrinter.printTo(sw, seq)
      sw.toString ===
        """[{
          |  "a": "a"
          |}, {
          |  "a": "b",
          |  "b": 1
          |}]""".stripMargin
    }
    "read a serialized seq of objects" in {
      PullParser.read[Seq[TestObject]]("""[{"a": "a"}, {"b": 1,"a":"b"}]""") === seq
    }
  }

  "immutableSetFormat" should {
    val set = Set(4, 5, 6)
    "serialize a Set[Int] to an array" in {
      val sw = new StringWriter
      PrettyStreamPrinter.printTo(sw, set)
      sw.toString === "[4, 5, 6]"
    }
    "read an array of numbers as a Set[Int]" in {
      PullParser.read[Set[Int]]("[4,5,6]") === set
    }
  }

  "indexedSeqFormat" should {
    val seq = collection.IndexedSeq(3, 2, 1)
    "serialize an IndexedSeq[Int] to an array" in {
      val sw = new StringWriter
      PrettyStreamPrinter.printTo(sw, seq)
      sw.toString === "[3, 2, 1]"
    }
    "read an array of numbers as an IndexedSeq[Int]" in {
      PullParser.read[IndexedSeq[Int]]("[3,2,1]") === seq
    }
  }
}
