package jt

/*
trait Show[T] {
  def show(t: T): String
}
*/

object ShowMain extends App {
  import cats._
  import cats.syntax.all._

  case class Person(name: String)
  case class Car(model: String)

  implicit val personShow: Show[Person] = Show.show[Person](_.name)
  implicit val carShow: Show[Car] = Show.show[Car](_.model)

  val p = Person("jintao")
  println(p.show)
  
    
}