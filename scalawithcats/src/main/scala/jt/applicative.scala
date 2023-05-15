package applicative

object Main extends App {
  import cats.Applicative
  import cats.syntax.all._

  def product3[F[_]: Applicative, A, B, C](fa: F[A], fb: F[B], fc: F[C]): F[(A,B,C)] = {
    val F = Applicative[F]
    val fabc = F.product(F.product(fa, fb), fc)
    F.map(fabc){ case ((a, b), c) => (a, b, c)}
  }

  def product3_v1[F[_]: Applicative, A, B, C](fa: F[A], fb: F[B], fc: F[C]): F[(A,B,C)] = {
      (fa product fb product fc) map { case ((a, b), c) => (a, b, c)}
  }

  import cats.data.Nested
  import scala.concurrent.Future
  import scala.concurrent.ExecutionContext.Implicits.global

  val x: Future[Option[Int]] = Future.successful(Some(45))
  val y: Future[Option[Char]] = Future.successful(Some('a'))

  val composed = Applicative[Future].compose[Option].map2(x, y)(_ + _)
  val nested = Applicative[Nested[Future, Option, *]].map2(Nested(x), Nested(y))(_ + _)

  import java.sql.Connection
  val username: Option[String] = Some("u")
  val password: Option[String] = Some("p")
  val url: Option[String] = Some("jdbc://...")

  def attemptConnect(username: String, password: String, url: String): Option[Connection] = None

  val connOpt: Option[Option[Connection]] = Applicative[Option].map3(username, password, url)(attemptConnect)

  def traverseOption[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    as.foldRight(Some(List.empty[B]): Option[List[B]]){ (a: A, acc: Option[List[B]]) => (f(a), acc).mapN(_ :: _) }

  def traverseEither[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as.foldRight(Right(List.empty[B]): Either[E, List[B]]){ (a: A, acc: Either[E, List[B]]) => (f(a), acc).mapN(_ :: _) }

  def traverse[F[_]: Applicative, A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(Applicative[F].pure(List.empty[B])){ (a: A, acc: F[List[B]]) => (f(a), acc).mapN(_ :: _) }

}

