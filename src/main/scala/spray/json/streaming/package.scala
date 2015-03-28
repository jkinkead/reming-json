package spray.json

package object streaming {
  def jsonStreamReader[T](implicit reader: JsonStreamReader[T]) = reader
  def jsonStreamWriter[T](implicit writer: JsonStreamWriter[T]) = writer
  def jsonStreamFormat[T](implicit format: JsonStreamFormat[T]) = format
}
