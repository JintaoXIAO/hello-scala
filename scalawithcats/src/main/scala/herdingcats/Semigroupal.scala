package jt

object SemigroupalMain extends App {

  import cats._
  import cats.syntax.all._
  val hs = Functor[List].map(List(1,2,3)) ({ (_: Int) * (_: Int) }.curried)
  val v = Functor[List].map(hs){ _(8) }
  println(v)

  val a = Some(10)  
  val b = Some(20)
  val c = Semigroupal[Option].product(a, b) map { case (i,j) => i + j }
  println(c)

  val v1 = List(1,2,3)
  val v2 = List("a", "b", "c")
  val v3 = Semigroupal[List].product(v1, v2)
  println(v3)
}

/*
trait Semigroupal[F[_]] {
  def product[A,B](fa: F[A], fb: F[B]): F[(A, B)]
}
*/