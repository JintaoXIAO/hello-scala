package herdingcats

object SemigroupMain extends App {
  import cats._
  import cats.syntax.all._

  val v1 = List(1,2,3) |+| List(3,4,5)  
  println(v1)
}

/*
trait Semigroup[A] {
  def combine(x: A, y: A): A
}
*/