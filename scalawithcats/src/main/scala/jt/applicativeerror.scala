package applicativeerror

object Main {
  def attemptDivide(x: Int, y: Int): Either[String, Int] = {
    if(y == 0) Left("divisor is zero")
    else Right(x / y)
  }

  import cats._
  import cats.syntax.all._

  def attemptDivideApplicationError[F[_]](x: Int, y: Int)(implicit ae: ApplicativeError[F, String]): F[Int] =
    if (y == 0) ae.raiseError("divisor is errror")
    else ae.pure(x / y)

  type OnError[A] = Either[String, A]
  val e: OnError[Int] = attemptDivideApplicationError[OnError](30, 10)

  import cats.data.Validated
  type MyValidated[A] = Validated[String, A]

  val g = attemptDivideApplicationError[MyValidated](10, 20)
  val j = attemptDivideApplicationError[Validated[String, *]](30, 20)


}
