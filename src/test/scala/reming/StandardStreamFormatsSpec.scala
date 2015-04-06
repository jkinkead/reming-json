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

class StandardStreamFormatsSpec extends Specification {
  import DefaultStreamProtocol._

  "Option format" should {
    "read None" in {
      PullParser.read[Option[String]]("null") === None
    }
    "write None" in {
      val sw = new StringWriter
      PrettyStreamPrinter.printTo[Option[String]](sw, None)
      sw.toString === "null"
    }
    "read Some" in {
      PullParser.read[Option[String]](""""Exists"""") === Some("Exists")
    }
    "write Some" in {
      val sw = new StringWriter
      PrettyStreamPrinter.printTo[Option[String]](sw, Some("foo"))
      sw.toString === """"foo""""
    }
  }
  "Either format" should {
    "read Right" in {
      PullParser.read[Either[String, Int]]("[1, 123]") === Right(123)
    }
    "read Left" in {
      PullParser.read[Either[String, Int]]("""[0, "str"]""") === Left("str")
    }
  }

  "Tuple1 format" should {
    "read values" in {
      PullParser.read[Tuple1[Int]]("22") mustEqual Tuple1(22)
    }
  }
  "Tuple2 format" should {
    "read values" in {
      PullParser.read[(Int, Double)]("[22, 1.0]") mustEqual (22, 1.0)
    }
  }
  "Tuple3 format" should {
    "read values" in {
      PullParser.read[(Int, Double, String)]("""[22, 1.0, "str"]""") mustEqual (22, 1.0, "str")
    }
    "write values" in {
      val sw = new StringWriter
      PrettyStreamPrinter.printTo(sw, (22, 1.0, "str"))
      sw.toString mustEqual """[22, 1.0, "str"]"""
    }
  }
  "Tuple4 format" should {
    "read values" in {
      PullParser.read[(Int, Double, String, Int)]("""[22, 1.0, "str", 42]""") mustEqual
        (22, 1.0, "str", 42)
    }
  }
  "Tuple5 format" should {
    "read values" in {
      PullParser.read[(Int, Double, String, Int, Int)]("""[22, 1.0, "str", 42, 41]""") mustEqual
        (22, 1.0, "str", 42, 41)
    }
  }
  "Tuple6 format" should {
    "read values" in {
      PullParser.read[(Int, Double, String, Int, Int, Int)](
        """[22, 1.0, "str", 42, 41, 40]"""
      ) mustEqual (22, 1.0, "str", 42, 41, 40)
    }
  }
  "Tuple7 format" should {
    "read values" in {
      PullParser.read[(Int, Double, String, Int, Int, Int, String)](
        """[22, 1.0, "str", 42, 41, 40, "i"]"""
      ) mustEqual (22, 1.0, "str", 42, 41, 40, "i")
    }
  }
}
