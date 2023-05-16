package herdingcats


/*
trait Applicative[F[_]] {
  def pure[A](x: A): F[A]
}
*/
object ApplicativeMain extends App {
  import cats._
  import cats.syntax.all._

  def sequenceA[F[_]: Applicative, A](list: List[F[A]]): F[List[A]] = list match {
    case Nil => Applicative[F].pure(Nil: List[A])
    case x::xs => (x, sequenceA(xs)) mapN { _ :: _ }
  }

  val v1 = sequenceA(List(1.some, 2.some))  
  println(v1)

  val v2 = sequenceA(List(List(1,2,3), List(1,2,3)))
  println(v2)

}