package jt

/*
trait FlatMap[F[_]] extends Apply[F] {
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
}
*/

object FlatMapMain extends App {
  import cats._
  import cats.syntax.all._

  val v = (Right(3): Either[String, Int]) flatMap (x => Right(x + 1))  
  println(v)

  val v2 = "wisdom".some map { _ + "!" }
  println(v2)
}