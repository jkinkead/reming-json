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

class AdditionalFormatsSpec extends FlatSpec {
  import DefaultJsonProtocol._

  sealed trait Parent
  case class Child1(a: String) extends Parent
  object Child1 {
    implicit val child1Format = jsonFormat1(Child1.apply)
  }
  case class Child2(b: String, c: Int) extends Parent
  object Child2 {
    implicit val child2Format = jsonFormat2(Child2.apply)
  }

  object Parent {
    implicit val parentJsonFormat = parentFormat[Parent](
      childFormat[Child1, Parent], childFormat[Child2, Parent]
    )
  }

  val one: Parent = Child1("one")
  val two: Parent = Child2("two", 2)

  "polymorphic serialization" should "serialize the first child class" in {
    CompactPrinter.printToString(one) === """["Child1",{"a":"one"}]"""
  }

  it should "serialize the second child class" in {
    CompactPrinter.printToString(two) === """["Child2",{"b":"two","c":2}]"""
  }

  it should "read a serialized instance" in {
    JsonParser.read[Parent]("""["Child1",{"a":"value"}]""") === Child1("value")
  }

  it should "round-trip a Seq" in {
    val seq: Seq[Parent] = Seq(one, two)
    JsonParser.read[Seq[Parent]](CompactPrinter.printToString(seq)) === seq
  }
}
