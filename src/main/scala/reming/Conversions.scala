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

import java.io.Writer

/** Implicit conversions to make serializing and deserializing simpler. */
object Conversions {
  /** Wraps anything, giving it serilization and deserialization methods. Note that we don't
    * constrain `T` to have a JsonWriter, since that gives worse compiler errors.
    */
  implicit class WrapAny[T](val value: T) extends AnyVal {
    def prettyPrintTo(writer: Writer)(implicit jsonWriter: JsonWriter[T]): Unit = {
      PrettyPrinter.printTo(writer, value)
    }
    def prettyPrintToString(implicit jsonWriter: JsonWriter[T]): String = {
      PrettyPrinter.printToString(value)
    }
    def compactPrintTo(writer: Writer)(implicit jsonWriter: JsonWriter[T]): Unit = {
      CompactPrinter.printTo(writer, value)
    }
    def compactPrintToString(implicit jsonWriter: JsonWriter[T]): String = {
      CompactPrinter.printToString(value)
    }
  }
}
