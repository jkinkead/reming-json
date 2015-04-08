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

class ProductFormatsSpec extends BaseSpec {
  import DefaultJsonProtocol._

  case class Test0()
  object Test0 {
    implicit val test0Format = jsonFormat0(Test0.apply)
  }
  case class Test2(a: Int, b: Option[Double])
  object Test2 {
    implicit val test2Format = jsonFormat2(Test2.apply)
  }
  case class Test5(a: Int, b: Double, c: String, d: String, e: Int)
  object Test5 {
    implicit val test5Format = jsonFormat5(Test5.apply)
  }
  case class Nested(a: Test0, b: Test2)
  object Nested {
    implicit val testNestedFormat = jsonFormat2(Nested.apply)
  }

  "A JsonFormat created with 'jsonFormat0'" should "write an object" in {
    PrettyPrinter.printToString(Test0()) shouldBe "{\n\n}"
  }
  it should "read an object" in {
    JsonParser.read[Test0]("{}") shouldBe Test0()
  }

  "A JsonFormat created with 'jsonFormat2'" should "write a value with Some" in {
    CompactPrinter.printToString(Test2(1, Some(1.2))) shouldBe """{"a":1,"b":1.2}"""
  }
  it should "write a value with None" in {
    CompactPrinter.printToString(Test2(22, None)) shouldBe """{"a":22}"""
  }
  it should "read a value with Some" in {
    JsonParser.read[Test2]("""{ "b": 2.2, "a": 2}""") shouldBe Test2(2, Some(2.2))
  }
  it should "read a value with None" in {
    JsonParser.read[Test2]("""{"a": 2}""") shouldBe Test2(2, None)
  }

  "A JsonFormat created with 'jsonFormat5'" should "write a value" in {
    CompactPrinter.printToString(Test5(33, 3.3, "foo", "bar", 44)) shouldBe
      """{"a":33,"b":3.3,"c":"foo","d":"bar","e":44}"""
  }
  it should "read a value" in {
    JsonParser.read[Test5]("""{"b": 3.3,"e":44,"a":33,"d":"bar","c":"foo"}""") shouldBe
      Test5(33, 3.3, "foo", "bar", 44)
  }

  "a nested case class" should "write a value" in {
    CompactPrinter.printToString(Nested(Test0(), Test2(12, Some(3.4)))) shouldBe
      """{"a":{},"b":{"a":12,"b":3.4}}"""
  }
  it should "read a value" in {
    JsonParser.read[Nested]("""{"b":{"a":12,"b":3.4},"a":{}}""") shouldBe
      Nested(Test0(), Test2(12, Some(3.4)))
  }
}
