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

import scala.collection.SortedMap

class CollectionFormatsSpec extends FlatSpec {
  import DefaultJsonProtocol._

  "listFormat" should "serialize a List[Int] to an array" in {
    PrettyPrinter.printToString(List(1, 2, 3)) === "[1, 2, 3]"
  }
  it should "read a serialized int array" in {
    JsonParser.read[List[Int]]("[1,2,3]") === List(1, 2, 3)
  }

  "arrayFormat" should "serialize an Array[Int] to an int array" in {
    PrettyPrinter.printToString(Array(1, 2, 3)) === "[1, 2, 3]"
  }
  it should "read a serialized int array" in {
    JsonParser.read[Array[Int]]("[1,2,3]") === Array(1, 2, 3)
  }

  "stringMapFormat" should "serialize a Map[String, Long] to an object" in {
    val map = Map("a" -> 1, "b" -> 2, "c" -> 3)
    PrettyPrinter.printToString(map) === """{
        |  "a": 1,
        |  "b": 2,
        |  "c": 3
        |}""".stripMargin
  }
  it should "read an object with long values to a Map[String, Long]" in {
    val map = Map("a" -> 1, "b" -> 2, "c" -> 3)
    JsonParser.read[Map[String, Long]]("""{"a":1,"c":3,"b":2}""") === map
  }

  "anyMapFormat" should "serialize a Map[Int, String] to an array of tuples" in {
    val map = SortedMap(1 -> "a", 2 -> "b", 3 -> "c")
    PrettyPrinter.printToString(map) === """[[1, "a"], [2, "b"], [3, "c"]]"""
  }
  it should "read an object with long values to a Map[Int, String]" in {
    val map = Map(1 -> "a", 2 -> "b", 3 -> "c")
    JsonParser.read[Map[Int, String]]("""[[1, "a"], [2, "b"], [3, "c"]]""") === map
  }

  case class TestObject(a: String, b: Option[Int])
  object TestObject {
    implicit val innerFormat = jsonFormat2(TestObject.apply)
  }
  "seq format" should "serialize a seq of objects" in {
    val seq = Seq(TestObject("a", None), TestObject("b", Some(1)))
    PrettyPrinter.printToString(seq) === """[{
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
    PrettyPrinter.printToString(set) === "[4, 5, 6]"
  }
  it should "read an array of numbers as a Set[Int]" in {
    val set = Set(4, 5, 6)
    JsonParser.read[Set[Int]]("[4,5,6]") === set
  }

  "indexedSeqFormat" should "serialize an IndexedSeq[Int] to an array" in {
    val seq = collection.IndexedSeq(3, 2, 1)
    PrettyPrinter.printToString(seq) === "[3, 2, 1]"
  }
  it should "read an array of numbers as an IndexedSeq[Int]" in {
    val seq = collection.IndexedSeq(3, 2, 1)
    JsonParser.read[IndexedSeq[Int]]("[3,2,1]") === seq
  }
}
