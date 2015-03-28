package spray.json.streaming

import spray.json.deserializationError

import scala.util.{ Failure, Success, Try }

/** Provides formats for Option, Either, and Tuple{1-7}. Option reads 'null' as None, Some
  * otherwise. 'Either' attempts to read the Right and Left types, returning whichever one
  * unabiguously parses. Tuple1 serializes as a direct literal, while Tuple{2-7} serialize as
  * arrays.
  */
trait StandardStreamFormats { self: BasicStreamFormats =>
  private[json] type JSF[T] = JsonStreamFormat[T] // simple alias for reduced verbosity

  implicit def optionFormat[T : JSF] = new OptionFormat[T]

  class OptionFormat[T : JSF] extends JSF[Option[T]] {
    override def write(value: Option[T], printer: JsonStreamPrinter): Unit = value match {
      case Some(v) => printer.print(v)
      case None => printer.printNull()
    }
    override def read(parser: PullParser) = if (parser.maybeReadNull()) {
      None
    } else {
      Some(parser.read[T])
    }
  }

  /** Format for Either. This is *NOT* compatible with spray-json. */
  implicit def eitherFormat[A : JSF, B : JSF] = new JSF[Either[A, B]] {
    override def write(value: Either[A, B], printer: JsonStreamPrinter): Unit = {
      printer.startArray()
      value match {
        case Left(a) =>
          printer.printArrayItem(0)
          printer.print(a)
        case Right(b) =>
          printer.printArrayItem(1)
          printer.print(b)
      }
      printer.endArray()
    }
    override def read(parser: PullParser) = {
      val sigil = parser.startArray[Int]()
      val value = sigil match {
        case 0 => Left(parser.readArrayItem[A])
        case _ => Right(parser.readArrayItem[B])
      }
      parser.endArray()
      value
    }
  }

  implicit def tuple1Format[A : JSF] = new JSF[Tuple1[A]] {
    override def write(value: Tuple1[A], printer: JsonStreamPrinter): Unit = printer.print(value._1)
    override def read(parser: PullParser) = Tuple1(parser.read[A])
  }

  implicit def tuple2Format[A : JSF, B : JSF] = new JSF[(A, B)] {
    override def write(value: (A, B), printer: JsonStreamPrinter): Unit = {
      printer.startArray()
      printer.printArrayItem(value._1)
      printer.printArrayItem(value._2)
      printer.endArray()
    }
    override def read(parser: PullParser) = {
      val result = (parser.startArray[A], parser.readArrayItem[B])
      parser.endArray
      result
    }
  }

  implicit def tuple3Format[A : JSF, B : JSF, C : JSF] = new JSF[(A, B, C)] {
    override def write(value: (A, B, C), printer: JsonStreamPrinter): Unit = {
      printer.startArray()
      printer.printArrayItem(value._1)
      printer.printArrayItem(value._2)
      printer.printArrayItem(value._3)
      printer.endArray()
    }
    override def read(parser: PullParser) = {
      val result = (parser.startArray[A], parser.readArrayItem[B], parser.readArrayItem[C])
      parser.endArray
      result
    }
  }

  implicit def tuple4Format[A : JSF, B : JSF, C : JSF, D : JSF] = new JSF[(A, B, C, D)] {
    override def write(value: (A, B, C, D), printer: JsonStreamPrinter): Unit = {
      printer.startArray()
      printer.printArrayItem(value._1)
      printer.printArrayItem(value._2)
      printer.printArrayItem(value._3)
      printer.printArrayItem(value._4)
      printer.endArray()
    }
    override def read(parser: PullParser) = {
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

  implicit def tuple5Format[A : JSF, B : JSF, C : JSF, D : JSF, E : JSF] = {
    new JSF[(A, B, C, D, E)] {
      override def write(value: (A, B, C, D, E), printer: JsonStreamPrinter): Unit = {
        printer.startArray()
        printer.printArrayItem(value._1)
        printer.printArrayItem(value._2)
        printer.printArrayItem(value._3)
        printer.printArrayItem(value._4)
        printer.printArrayItem(value._5)
        printer.endArray()
      }
      override def read(parser: PullParser) = {
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

  implicit def tuple6Format[A : JSF, B : JSF, C : JSF, D : JSF, E : JSF, F : JSF] = {
    new JSF[(A, B, C, D, E, F)] {
      override def write(value: (A, B, C, D, E, F), printer: JsonStreamPrinter): Unit = {
        printer.startArray()
        printer.printArrayItem(value._1)
        printer.printArrayItem(value._2)
        printer.printArrayItem(value._3)
        printer.printArrayItem(value._4)
        printer.printArrayItem(value._5)
        printer.printArrayItem(value._6)
        printer.endArray()
      }
      override def read(parser: PullParser) = {
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

  implicit def tuple7Format[A : JSF, B : JSF, C : JSF, D : JSF, E : JSF, F : JSF, G : JSF] = {
    new JSF[(A, B, C, D, E, F, G)] {
      override def write(value: (A, B, C, D, E, F, G), printer: JsonStreamPrinter): Unit = {
        printer.startArray()
        printer.printArrayItem(value._1)
        printer.printArrayItem(value._2)
        printer.printArrayItem(value._3)
        printer.printArrayItem(value._4)
        printer.printArrayItem(value._5)
        printer.printArrayItem(value._6)
        printer.printArrayItem(value._7)
        printer.endArray()
      }
      override def read(parser: PullParser) = {
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
