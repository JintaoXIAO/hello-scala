package herdingcats

/*
trait Eq[@specialized A] extends Any with Serializable { self =>
  def eqv(x: A, y: A): Boolean
  def neqv(x: A, y: A): Boolean = !eqv(x, y)  
}
*/

object EqMain extends App {
  import cats._
  import cats.syntax.all._

  println(1 === 1)  
  println(
    (Some(1): Option[Int]) =!= (Some(2): Option[Int])
  )
}