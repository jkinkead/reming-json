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

class StandardFormatsSpec extends BaseSpec {
  import DefaultJsonProtocol._

  "Option format" should "read None" in {
    JsonParser.read[Option[String]]("null") shouldBe None
  }
  it should "write None" in {
    PrettyPrinter.printToString[Option[String]](None) shouldBe "null"
  }
  it should "read Some" in {
    JsonParser.read[Option[String]](""""Exists"""") shouldBe Some("Exists")
  }
  it should "write Some" in {
    PrettyPrinter.printToString[Option[String]](Some("foo")) shouldBe """"foo""""
  }

  "Either format" should "read Right" in {
    JsonParser.read[Either[String, Int]]("[1, 123]") shouldBe Right(123)
  }
  it should "read Left" in {
    JsonParser.read[Either[String, Int]]("""[0, "str"]""") shouldBe Left("str")
  }

  "Tuple1 format" should "read values" in {
    JsonParser.read[Tuple1[Int]]("22") shouldBe Tuple1(22)
  }

  "Tuple2 format" should "read values" in {
    JsonParser.read[(Int, Double)]("[22, 1.0]") shouldBe (22, 1.0)
  }

  "Tuple3 format" should "read values" in {
    JsonParser.read[(Int, Double, String)]("""[22, 1.0, "str"]""") shouldBe (22, 1.0, "str")
  }
  it should "write values" in {
    PrettyPrinter.printToString((22, 1.0, "str")) shouldBe """[22, 1.0, "str"]"""
  }

  "Tuple4 format" should "read values" in {
    JsonParser.read[(Int, Double, String, Int)]("""[22, 1.0, "str", 42]""") shouldBe
      (22, 1.0, "str", 42)
  }

  "Tuple5 format" should "read values" in {
    JsonParser.read[(Int, Double, String, Int, Int)]("""[22, 1.0, "str", 42, 41]""") shouldBe
      (22, 1.0, "str", 42, 41)
  }

  "Tuple6 format" should "read values" in {
    JsonParser.read[(Int, Double, String, Int, Int, Int)](
      """[22, 1.0, "str", 42, 41, 40]"""
    ) shouldBe (22, 1.0, "str", 42, 41, 40)
  }

  "Tuple7 format" should "read values" in {
    JsonParser.read[(Int, Double, String, Int, Int, Int, String)](
      """[22, 1.0, "str", 42, 41, 40, "i"]"""
    ) shouldBe (22, 1.0, "str", 42, 41, 40, "i")
  }
}
