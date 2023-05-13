package misc

object Main {
  trait Semigroup[A] {
    def combine(a1: A, a2: A): A
  }

  trait SemigroupK[F[_]] {
    def combineK[A](fa1: F[A], fa2: F[A]): F[A]
  }

  trait Monoid[A] extends Semigroup[A] {
    def empty: A
  }

  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]

    def lift[A,B](f: A => B): F[A] => F[B] = fa => map(fa)(f)
  }



}