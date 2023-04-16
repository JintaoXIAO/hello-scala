package json

sealed trait Json

final case class JsObject(get: Map[String, Json]) extends Json

final case class JsString(get: String) extends Json

final case class JsNumber(get: Double) extends Json

final case object JsNull extends Json

trait JsonWriter[A] {
  def write(value: A): Json
}

final case class Person(name: String, email: String)

object JsonWriterInstances {
  implicit val stringWriter: JsonWriter[String] = new JsonWriter[String] {
    def write(value: String): Json = JsString(value)
  }
  implicit val personWriter: JsonWriter[Person] = new JsonWriter[Person] {
    def write(value: Person): Json = JsObject(Map(
      "name" -> JsString(value.name),
      "email" -> JsString(value.email)
    ))
  }

  implicit def optionWriter[A](implicit writer: JsonWriter[A]): JsonWriter[Option[A]] =
    new JsonWriter[Option[A]] {
      def write(value: Option[A]): Json = value match {
        case Some(a) => writer.write(a)
        case None => JsNull
      }
    }
}

object JsonSyntx {
  implicit class JsonWriterOps[A](value: A) {
    def toJson(implicit w: JsonWriter[A]): Json = w.write(value)
  }
}

object Json {
  def toJson[A](value: A)(implicit w: JsonWriter[A]): Json = w.write(value)
}

object Main extends App {

  import JsonSyntx._
  import JsonWriterInstances._
  import cats.instances.option._
  import cats.syntax.option._

  // val s = Person("dave", "dave@aabb.com").toJson
  // val s2 = Json.toJson(Option(Person("jt", "jt@scala.com")))
  // val s2 = Json.toJson(Person("jt", "jt@scala.com").some)
  val s2 = Json.toJson(Some(Person("jt", "jt@scala.com")).asInstanceOf[Option[Person]])
  println(s2)

  trait Printable[A] {
    def format(value: A): String
  }

  object PrintableInstances {
    val intPrintable: Printable[Int] = new Printable[Int] {
      def format(value: Int): String = value.toString
    }

    val strPrintable: Printable[String] = new Printable[String] {
      def format(value: String): String = value
    }
  }

  object Printable {
    def format[A](value: A)(implicit pr: Printable[A]) = pr.format(value)

    def print[A](value: A)(implicit pr: Printable[A]) = println(format(value))
  }

  final case class Cat(name: String, age: Int, color: String)

  object PrintableSyntax {
    implicit class PrintableOps[A](value: A) {
      def format(implicit pr: Printable[A]): String = pr.format(value)

      def print(implicit pr: Printable[A]): Unit = println(pr.format(value))
    }
  }

  object Main extends App {

    import PrintableSyntax._

    implicit val catPrintable: Printable[Cat] = new Printable[Cat] {
      def format(c: Cat): String = s"${c.name} is a ${c.age} year-old ${c.color} cat."
    }

    val cat = Cat("kitty", 1, "yellow")
    // Printable.print(cat)
    cat.print

    import java.util.Date
    import cats.Show
    import cats.syntax.show._

    implicit val dateShow: Show[Date] = Show.show[Date](date => s"this is a data: $date")
    implicit val catShow: Show[Cat] =
      Show.show[Cat](c => s"${c.name} is a ${c.age} year-old ${c.color} cat.")

    println(new Date().show)
    println(cat.show)
  }

}

  