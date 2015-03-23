package spray.json.streaming

import org.specs2.mutable._

import java.io.StringWriter

/** Tests the ability of the streaming API to consume its own output. */
class RoundTripSpec extends Specification {
  import DefaultStreamProtocol._

  case class Inner(innerA: String, innerB: Option[Int])
  object Inner {
    implicit val innerFormat = jsonStreamFormat2(Inner.apply)
  }
  case class Middle(a: Int, b: String, c: Seq[Int], d: Set[Inner])
  object Middle {
    implicit val middleFormat = jsonStreamFormat4(Middle.apply)
  }
  case class Outer(outerA: Seq[Inner], outerB: Middle)
  object Outer {
    implicit val outerFormat = jsonStreamFormat2(Outer.apply)
  }

  val testVal = Outer(
    Seq(Inner("first", None), Inner("second", Some(1))),
    Middle(
      22,
      "middle",
      Seq(1, 2, 3, 4, 5),
      Set(Inner("a", None), Inner("b", Some(3)), Inner("c", Some(5)))
    )
  )

  "Printing -> Parsing round-trip" should {
    "work when using pretty printer" in {
      val sw = new StringWriter
      PrettyStreamPrinter.printTo(sw, testVal)
      PullParser.read[Outer](sw.toString) === testVal
    }
  }
}
