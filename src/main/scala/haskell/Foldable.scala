package haskell

trait Foldable[F[_]] {
  def foldMap[M[_]: Monoid, A, B](f: A => M[B])(fa: F[A]): M[B]
  def foldr[A, B](f: A => B => B, b: B, fa: F[A]): B
}
