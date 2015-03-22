package spray.json.streaming

trait ProductStreamFormats extends ProductStreamFormatsInstances { this: StandardStreamFormats =>
  def jsonStreamFormat0[T](construct: () => T): JsonStreamFormat[T] = {
    new JsonStreamFormat[T] {
      def write(p: T, printer: JsonStreamPrinter): Unit = {
        printer.startObject()
        printer.endObject()
      }
      def read(parser: PullParser): T = {
        parser.startObject()
        parser.endObject()
        construct()
      }
    }
  }

  // helpers

  /** Writes the given field of a product. This special-cases None values using the default
    * serialization, omitting them from the serialization entirely. This is always called from
    * within an object serialization.
    * @param fieldName the name to serialize the field as
    * @param ix the index of the field to serialize in the product
    */
  protected def writeProductElement[T](
    fieldName: String,
    p: Product,
    ix: Int,
    printer: JsonStreamPrinter
  )(implicit writer: JsonStreamWriter[T]): Unit = {
    val value = p.productElement(ix).asInstanceOf[T]
    writer match {
      case _: OptionFormat[_] if (value == None) => // No-op
      case _ => printer.printField(fieldName, value)
    }
  }

  /** Reads a value from an object, treating a missing value as None if the format is for an Option.
    */
  protected def readObjectValue[T](
    value: ObjectValue[T]
  )(implicit reader: JsonStreamReader[T]): T = {
    if (reader.isInstanceOf[OptionFormat[_]] && value.optionalValue.isEmpty) {
      None.asInstanceOf[T]
    } else {
      value.value
    }
  }
}

/** This trait supplies an alternative rendering mode for optional case class members.
  * Normally optional members that are undefined (`None`) are not rendered at all.
  * By mixing in this trait into your custom JsonProtocol you can enforce the rendering of undefined members as `null`.
  * (Note that this only affect JSON writing, spray-json will always read missing optional members as well as `null`
  * optional members as `None`.)
  */
trait NullOptions extends ProductStreamFormats {
  this: StandardStreamFormats =>

  override protected def writeProductElement[T](
    fieldName: String,
    p: Product,
    ix: Int,
    printer: JsonStreamPrinter
  )(implicit writer: JsonStreamWriter[T]): Unit = {
    printer.printField(fieldName, p.productElement(ix).asInstanceOf[T])
  }
}
