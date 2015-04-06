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

class BasicFormatsSpec extends FlatSpec {
  import DefaultProtocol._

  "Int format" should "read an int" in {
    JsonParser.read[Int]("123") === 123
  }
  it should "write an int" in {
    val sw = new StringWriter
    PrettyPrinter.printTo(sw, 123)
    sw.toString === "123"
  }

  "String format" should "read a string" in {
    JsonParser.read[String](""""abc"""") === "abc"
  }
  it should "write a string" in {
    val sw = new StringWriter
    PrettyPrinter.printTo(sw, "abc")
    sw.toString === """"abc""""
  }
}
