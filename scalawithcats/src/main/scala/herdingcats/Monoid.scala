package jt

trait Monoid[A] {
  def mappend(a1: A, a2: A): A
  def mzero: A
}
object Monoid {

  object syntax extends MonoidSyntax

  implicit val intMonoid: Monoid[Int] = new Monoid[Int] {
    override def mappend(a1: Int, a2: Int): Int = a1 + a2
    override def mzero: Int = 0
  }
}

trait MonoidSyntax {
  implicit final def syntaxMonoid[A: Monoid](a: A): MonoidOps[A] =
    new MonoidOps[A](a)
}

final class MonoidOps[A: Monoid](lhs: A) {
  def |+|(rhs: A): A = implicitly[Monoid[A]].mappend(lhs, rhs)
}

object MonoidMain extends App {
  import Monoid.syntax._

  val v1 = 3 |+| 4  
  println(v1)
}