# Reming JSON Serializer

### _"Streaming JSON without the AST!"_

Reming is a streaming [JSON](http://json.org) serializer based on
[_spray-json_](https://github.com/spray/spray-json).  If you're familiar with _spray-json_, reming
should look extremely familiar to you - but you'll notice it's missing the intermediary AST that
_spray-json_ produces! This makes Reming better-suited for large documents, especially in terms of
memory overhead.

Like _spray-json_, it has:
* An efficient JSON parser
* Choice of either compact or pretty JSON-to-string printing
* Type-class based marshaling of custom objects (no reflection, no intrusion)
* No external dependencies

You should use Reming if:
* You want to marshal multi-megabyte JSON objects
* You want _spray-json_-style typeclass definitions, but want something more efficient
* You marshal only case classes, or are comfortable writing pull parser code

You should NOT use Reming if:
* You want to parse arbitrary JSON objects without having to define structure for them first (although this could be done with a bridge to another library)
* You have non-case-classes to marshal, and want to use an AST (JSON objects) as your abstraction

### Getting Started

Include the following dependency in your favorite `sbt` file:

```scala
libraryDependencies += "com.github.jkinkead" %% "reming-json" % "0.0.9-SNAPSHOT"
```

Reming is built against Scala 2.11.X.

### Usage

#### JsonFormat

In order to parse or produce JSON, Reming requires an implicit
[`reming.JsonFormat[T]`](src/main/scala/reming/JsonFormat.scala) for the type
being read or written. Reming provides `JsonFormat` instances for:

* [Basic JSON types](src/main/scala/reming/BasicFormats.scala) - Numbers, Boolean, Char, String, Symbol
* [Scala core library types](src/main/scala/reming/StandardFormats.scala) - Option, Either, Tuple{1-7}
* [Scala collections](src/main/scala/reming/CollectionFormats.scala) - Arrays, various Seqs and Iterables, Sets, and `Map[String, _]`

Additionally, Reming has support for automatically building JSON representation
of case classes, including support for polymorphic classes.

All of the builtin formats can be imported from `DefaultJsonProtocol`:

```scala
import reming.DefaultJsonProtocol._
```

You can then read and write basic types:

```scala
import reming.{ CompactPrinter, JsonParser }
import reming.DefaultJsonProtocol._

// Reming uses Writers for output. For best performance, make sure your underlying
// Writer or OutputStream is buffered.
import java.io.{ BufferedWriter, FileInputStream, FileWriter }

import scala.io.Source

val serializedAsObject = Map("key" -> 1, "anotherKey" -> 2)
val destination = new BufferedWriter(new FileWriter("outputfile.json")

// Or reming.PrettyPrinter.
CompactPrinter.printTo(destination, serializedAsObject)

// Reming uses Source for input. If you're wrapping a stream, use BufferedSource.
val deserialized = JsonParser.read[Map[String, Int]](Source.fromFile("outputfile.json"))
```

### Providing JsonFormats for Case Classes

If your custom type `T` is a case class, then adding a `JsonFormat[T]` is easy:

```scala
case class Color(name: String, red: Int, green: Int, blue: Int)

object Color {
  // This import gives you serializations for String & Int!
  import reming.DefaultJsonProtocol._

  // Use jsonFormatX, where 'X' is the number of arguments your constructor has.
  implicit val colorFormat = jsonFormat4(Color.apply)
}

// Convenience method for generating a string in-memory.
val jsonString = CompactPrinter.printToString(Color("CadetBlue", 95, 158, 160))

val color = JsonParser.read[Color](jsonString)
```

The `jsonFormatX` methods extract the field names of your case class, then call the general
`jsonFormat` function with those names. If Reming has trouble determining the field names, or if
your JSON objects use member names that differ from the case class fields, you can instead use
`jsonFormat` directly.

Declaring the format on the companion object for `T` will make the `JsonFormat[T]` be in the
implicit scope always, which can be convenient. Another option is to extend DefaultJsonProtocol
directly, and create all your formats there.

#### Case Classes with Type Parameters

If your case class takes type parameters, the `jsonFormat` methods require some extra boilerplate,
as you need to add context bounds for all type parameters:

```scala
case class NamedList[A](name: String, items: List[A])

object NamedList {
  import reming.DefaultJsonProtocol._

  // Reming can't serialize A unless it has a JsonFormat - so declare it as such!
  implicit def namedListFormat[A : JsonFormat] = jsonFormat2(NamedList.apply[A])
}
```

#### JsonFormats for Recursive Types

If your type is self-referentialy recursive, like:

```scala
case class Foo(i: Int, foo: Foo)
```

you need to wrap your format constructor with `lazyFormat` and supply an explicit type annotation:

```scala
implicit val fooFormat: JsonFormat[Foo] = lazyFormat(jsonFormat(Foo, "i", "foo"))
```

Otherwise your code will either not compile (no explicit type annotation) or throw an NPE at runtime
(no `lazyFormat` wrapper).

#### JsonFormats for Polymorphic Types

Reming includes helpers in `DefaultJsonProtocol` for creating serializations of a class with child
classes. To do so, create your child class `JsonFormat` instances as normal, and use the
`parentFormat` and `childFormat` helpers:

```scala
/** Parent class for animals. */
sealed trait Animal {
  def furriness: Double
}
object Animal {
  import reming.DefaultJsonProtocol._

  // Formats for Cat and Dog. These are marked private in this example. Often you want to avoid
  // serializing the children directly, since a Cat-serialized-as-Cat has a different format than a
  // Cat-serialized-as-Animal.
  private implicit val catFormat = jsonFormat2(Cat.apply)
  private implicit val dogFormat = jsonFormat2(Dog.apply)
  implicit val animalJsonFormat = parentFormat[Animal](
    childFormat[Cat, Animal], childFormat[Dog, Animal]
  )
}
/** Cats are meowy. */
case class Cat(furriness: Double, meowitude: Double) extends Animal
/** Dogs bark. */
case class Dog(furriness: Double, barkiness: Double) extends Animal
```

These will be serialized as two-element JSON arrays, with the first element being the name of the
runtime class (`Cat` or `Dog` in this example). You can also provide a name to `childFormat`, if
you want to use a custom one. This is mostly useful if you have long class names, and want something
with a shorter serialization:
```scala
  implicit val parentJsonFormat = parentFormat[Parent](
    childFormat[Cat, Animal]("c"), childFormat[Dog, Animal]("d")
  )
```

There's also a special `LazyFormat` class for use in serializing a child class that refers to its
parent:

```scala
sealed trait Animal {
  def furriness: Double
}
// Shark contains an instance of its parent class, Animal.
case class Shark(furriness: Double, favoriteSnack: Animal) extends Animal

object Animal {
  import reming.DefaultJsonProtocol._
  implicit object AnimalFormat extends LazyFormat[Animal] {
    // This has to be inside the LazyFormat, since the format for Shark will be using AnimalFormat
    // for its work.
    private implicit val sharkFormat = jsonFormat2(Shark.apply)

    // LazyFormat contains the implementation format in its "delegate" field.
    override val delegate = parentFormat[Animal](childFormat[Shark, Animal])
  }
}
```

#### NullOptions

The `NullOptions` trait supplies an alternative rendering mode for optional case class members.
Normally optional members that are undefined (`None`) are not rendered at all. By mixing in this
trait into your custom JsonProtocol you can enforce the rendering of undefined members as `null`.

(Note that this only affect JSON writing - Reming will always read missing or `null` optional members as `None`.)

### Providing JsonFormats for Other Types (Advanced)

Reming uses a pull parser and a JSON printer for reading and writing, respectively.

TODO: Document!

### Reming vs. the JSON specification

Reming doesn't guarantee a valid JSON file. In particular:

* Reming accepts any valid `JsonFormat` at the top-level, not just JSON objects
* Reming allows arbitrary trailing text in a JSON data stream (Reming doesn't assert that it's reached the end of a stream)

### Credits

Reming started as a fork of _spray-json_, written by
**[Mathias Doenitz](https://github.com/sirthias)**, with contributions by
**[Debasish Ghosh](https://github.com/debasishg)**. See the _spray-json_ credits page for details.

The polymorphic case class support is inspired by **[Mark Schaake](https://github.com/markschaake)**'s work on
[allenai/common](https://github.com/allenai/common/tree/a99c3c3a2991ac7c79eb9b2386d45fee73b1b38d/core/src/main/scala/org/allenai/common/json).

### License

Reming is licensed under [APL 2.0](http://www.apache.org/licenses/LICENSE-2.0)
