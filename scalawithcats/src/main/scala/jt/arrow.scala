package arrow

object Def {
  trait Arrow[F[_, _]] {
    def compose[A, B, C](f: F[B, C], g: F[A, B]): F[A, C]
    def first[A, B, C](fa: F[A, B]): F[(A, C), (B, C)]
    def lift[A, B](f: A => B): F[A, B]
  }
}

object Main {

  import cats.arrow.Arrow
  import cats.syntax.all._

  def combine[F[_, _]: Arrow, A, B, C](fab: F[A, B], fac: F[A, C]): F[A, (B, C)] =
    Arrow[F].lift((a: A) => (a, a)) >>> (fab *** fac)
  // Arrow[F].lift((a:A) => (a, a)): F[A, (A, A)]
  // fab *** fac : F[(A,A), (B,C)]
  // F[A,(A,A)] >>> F[(A,A), (B,C)]: F[A, (B,C)]

  val mean: List[Int] => Double = {
    // combine(F[List[Int], Int], F[List[Int],Int]
    // -> F[List[Int], (Int, Int)]
    // -> >>> F[(Int, Int), Double]
    // -> F[List[Int], Double]
    combine((_: List[Int]).sum, (_: List[Int]).size) >>> { case (x, y) => x.toDouble / y }
  }

  val variance: List[Int] => Double = {
    combine(((_: List[Int]).map(x => x * x)) >>> mean, mean) >>> { case (x, y) => x - y * y }
  }

  val meanAndVar: List[Int] => (Double, Double) = combine(mean, variance)



}
