package jt

/*
trait Foldable[F[_]] {
  def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B
  def fold[A](fa: F[A]): A = {
    val m = implicitly[Monoid[A]]
    foldLeft(fa, m.empty) { (acc, a) => m.combine(acc, a)}
  }
}
*/

object FoldableMain extends App {
  import cats._
  import cats.syntax.all._

  val v = Foldable[List].fold(List(1,2,3))(Monoid[Int])
  println(v) 
}