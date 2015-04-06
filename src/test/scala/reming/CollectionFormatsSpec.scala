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

import org.scalatest.FlatSpec

import java.io.StringWriter

class CollectionFormatsSpec extends FlatSpec {
  import DefaultProtocol._

  "listFormat" should "serialize a List[Int] to an array" in {
    val sw = new StringWriter
    PrettyPrinter.printTo(sw, List(1, 2, 3))
    sw.toString === "[1, 2, 3]"
  }
  it should "read a serialized int array" in {
    JsonParser.read[List[Int]]("[1,2,3]") === List(1, 2, 3)
  }

  "arrayFormat" should "serialize an Array[Int] to an int array" in {
    val sw = new StringWriter
    PrettyPrinter.printTo(sw, Array(1, 2, 3))
    sw.toString === "[1, 2, 3]"
  }
  it should "read a serialized int array" in {
    JsonParser.read[Array[Int]]("[1,2,3]") === Array(1, 2, 3)
  }

  "mapFormat" should "serialize a Map[String, Long] to an object" in {
    val map = Map("a" -> 1, "b" -> 2, "c" -> 3)
    val sw = new StringWriter
    PrettyPrinter.printTo(sw, map)
    sw.toString ===
      """{
        |  "a": 1,
        |  "b": 2,
        |  "c": 3
        |}""".stripMargin
  }
  it should "read an object with long values to a Map[String, Long]" in {
    val map = Map("a" -> 1, "b" -> 2, "c" -> 3)
    JsonParser.read[Map[String, Long]]("""{"a":1,"c":3,"b":2}""") === map
  }

  case class TestObject(a: String, b: Option[Int])
  object TestObject {
    implicit val innerFormat = jsonFormat2(TestObject.apply)
  }
  "seq format" should "serialize a seq of objects" in {
    val seq = Seq(TestObject("a", None), TestObject("b", Some(1)))
    val sw = new StringWriter
    PrettyPrinter.printTo(sw, seq)
    sw.toString ===
      """[{
        |  "a": "a"
        |}, {
        |  "a": "b",
        |  "b": 1
        |}]""".stripMargin
  }
  it should "read a serialized seq of objects" in {
    val seq = Seq(TestObject("a", None), TestObject("b", Some(1)))
    JsonParser.read[Seq[TestObject]]("""[{"a": "a"}, {"b": 1,"a":"b"}]""") === seq
  }

  "immutableSetFormat" should "serialize a Set[Int] to an array" in {
    val set = Set(4, 5, 6)
    val sw = new StringWriter
    PrettyPrinter.printTo(sw, set)
    sw.toString === "[4, 5, 6]"
  }
  it should "read an array of numbers as a Set[Int]" in {
    val set = Set(4, 5, 6)
    JsonParser.read[Set[Int]]("[4,5,6]") === set
  }

  "indexedSeqFormat" should "serialize an IndexedSeq[Int] to an array" in {
    val seq = collection.IndexedSeq(3, 2, 1)
    val sw = new StringWriter
    PrettyPrinter.printTo(sw, seq)
    sw.toString === "[3, 2, 1]"
  }
  it should "read an array of numbers as an IndexedSeq[Int]" in {
    val seq = collection.IndexedSeq(3, 2, 1)
    JsonParser.read[IndexedSeq[Int]]("[3,2,1]") === seq
  }
}
