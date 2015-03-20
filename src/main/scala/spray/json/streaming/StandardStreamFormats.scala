package spray.json.streaming

import spray.json.deserializationError
import spray.json.streaming.AdditionalStreamFormats.safeReader

import scala.util.{ Failure, Success, Try }

/** Provides formats for Option, Either, and Tuple{1-7}. Option reads 'null' as None, Some
  * otherwise. 'Either' attempts to read the Right and Left types, returning whichever one
  * unabiguously parses. Tuple1 serializes as a direct literal, while Tuple{2-7} serialize as
  * arrays.
  */
trait StandardStreamFormats {
  private[json] type JSR[T] = JsonStreamReader[T] // simple alias for reduced verbosity

  implicit def optionFormat[T : JsonStreamReader] = new OptionFormat[T]

  class OptionFormat[T : JSR] extends JSR[Option[T]] {
    override def read(parser: PullParser) = if (parser.maybeReadNull()) {
      None
    } else {
      Some(parser.read[T])
    }
  }

  implicit def eitherFormat[A : JSR, B : JSR] = new JSR[Either[A, B]] {
    def read(parser: PullParser) = {
      (parser.read()(safeReader[A]), parser.read()(safeReader[B])) match {
        case (Success(a), _: Failure[_]) => Left(a)
        case (_: Failure[_], Success(b)) => Right(b)
        case (_: Success[_], _: Success[_]) => deserializationError("Ambiguous Either value: can be read as both, Left and Right, values")
        case (Failure(ea), Failure(eb)) => deserializationError("Could not read Either value:\n" + ea + "---------- and ----------\n" + eb)
      }
    }
  }
  
  implicit def tuple1Format[A :JSR] = new JSR[Tuple1[A]] {
    def read(parser: PullParser) = Tuple1(parser.read[A])
  }
  
  implicit def tuple2Format[A :JSR, B :JSR] = new JSR[(A, B)] {
    def read(parser: PullParser) = {
      val result = (parser.startArray[A], parser.readArrayItem[B])
      parser.endArray
      result
    }
  }
  
  implicit def tuple3Format[A :JSR, B :JSR, C :JSR] = new JSR[(A, B, C)] {
    def read(parser: PullParser) = {
      val result = (parser.startArray[A], parser.readArrayItem[B], parser.readArrayItem[C])
      parser.endArray
      result
    }
  }
  
  implicit def tuple4Format[A :JSR, B :JSR, C :JSR, D :JSR] = new JSR[(A, B, C, D)] {
    def read(parser: PullParser) = {
      val result = (
        parser.startArray[A],
        parser.readArrayItem[B],
        parser.readArrayItem[C],
        parser.readArrayItem[D]
      )
      parser.endArray
      result
    }
  }
  
  implicit def tuple5Format[A :JSR, B :JSR, C :JSR, D :JSR, E :JSR] = {
    new JSR[(A, B, C, D, E)] {
      def read(parser: PullParser) = {
        val result = (
          parser.startArray[A],
          parser.readArrayItem[B],
          parser.readArrayItem[C],
          parser.readArrayItem[D],
          parser.readArrayItem[E]
        )
        parser.endArray
        result
      }
    }
  }
  
  implicit def tuple6Format[A :JSR, B :JSR, C :JSR, D :JSR, E :JSR, F: JSR] = {
    new JSR[(A, B, C, D, E, F)] {
      def read(parser: PullParser) = {
        val result = (
          parser.startArray[A],
          parser.readArrayItem[B],
          parser.readArrayItem[C],
          parser.readArrayItem[D],
          parser.readArrayItem[E],
          parser.readArrayItem[F]
        )
        parser.endArray
        result
      }
    }
  }
  
  implicit def tuple7Format[A :JSR, B :JSR, C :JSR, D :JSR, E :JSR, F: JSR, G: JSR] = {
    new JSR[(A, B, C, D, E, F, G)] {
      def read(parser: PullParser) = {
        val result = (
          parser.startArray[A],
          parser.readArrayItem[B],
          parser.readArrayItem[C],
          parser.readArrayItem[D],
          parser.readArrayItem[E],
          parser.readArrayItem[F],
          parser.readArrayItem[G]
        )
        parser.endArray
        result
      }
    }
  }
}
