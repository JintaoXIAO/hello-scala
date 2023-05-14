package jt

object FunctorMain extends App {
  import cats.Functor
  import cats.syntax.functor._

  val v1 = (Right(1): Either[String, Int]) map { _ + 1 } 
  println(v1)

  val add1: Int => Int = (x: Int) => x + 1
  val h: Int => Int = add1 map { _ * 6 }

  println(h(10))

  val lifted = Functor[List].lift { (_: Int) * 2 }
  val v2 = lifted(List(1,2,3))
  println(v2)

  val v3 = List(1,2,3) fproduct { _ * 2 }
  println(v3)
}