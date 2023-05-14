package jt

/*
trait Apply[F[_]] {
  def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]
}
*/

object ApplyMain extends App {
  import cats._
  import cats.syntax.all._

  val v = (List("ha", "heh", "hmm"), List("?", "!", ".")) mapN { _ + _ } 
  println(v)

  val v1 = (1.some <* 2.some)  
  println(v1)

  val v2 = (1.some <* none[Int])  
  println(v2)

  val v3 = (1.some *> 2.some)  
  println(v3)

  val v4 = (none[Int] *> 1.some)  
  println(v4)

  val v5 = ((_: Int) + 3).some ap 4.some
  println(v5)   
}