package monadt

object MonadT  extends App{
  case class User(name: String)
  def lookupUser(id: Long): Either[Error, Option[User]] = ???

  def lookupUserName(id: Long): Either[Error, Option[String]] =
    for {
      optUser <- lookupUser(id)
    } yield {
      for { user <- optUser } yield user.name
    }
  import cats.data.OptionT

  type ListOption[A] = OptionT[List, A]

  import cats.instances.list._
  import cats.syntax.applicative._

  val result1: ListOption[Int] = OptionT(List(Option(10), Option(20)))
  val result2: ListOption[Int] = 30.pure[ListOption]

  val r1 = for {
    a <- result1
    b <- result2
  } yield a + b
  println(r1)

  type ErrorOr[A] = Either[String, A]
  type ErrorOrOption[A] = OptionT[ErrorOr, A]
  import cats.instances.either._

  val a = 10.pure[ErrorOrOption]
  val b = 32.pure[ErrorOrOption]
  val c = a.flatMap(x => b.map(y => x + y))

  import scala.concurrent.Future
  import cats.data.{ EitherT, OptionT }

  type FutureEither[A] = EitherT[Future, String, A]
  type FutureEitherOption[A] = OptionT[FutureEither, A]

  import cats.instances.future._
  import scala.concurrent.Await
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration._

  val futureEitherOr: FutureEitherOption[Int] = for {
    a <- 10.pure[FutureEitherOption]
    b <- 32.pure[FutureEitherOption]
  } yield a + b

  val errorStack1 = OptionT[ErrorOr, Int](Right(Some(10)))
  val errorStack2 = 32.pure[ErrorOrOption]

  println(errorStack1.value)
  println(errorStack2)

  val intermediate: FutureEither[Option[Int]] = futureEitherOr.value
  val stack: Future[Either[String, Option[Int]]] = intermediate.value

  val r = Await.result(stack, 1.second)
  println(r)


  import cats.data.Writer

  type Logged[A] = Writer[List[String], A]

  def parseNumber(str: String): Logged[Option[Int]] =
    util.Try(str.toInt).toOption match {
      case Some(num) => Writer(List(s"Read $str"), Some(num))
      case None => Writer(List(s"Failed on $str"), None)
    }

  def addAll(a: String, b: String, c: String): Logged[Option[Int]] = {
    import cats.data.OptionT

    val result = for {
      a <- OptionT(parseNumber(a))
      b <- OptionT(parseNumber(b))
      c <- OptionT(parseNumber(c))
    } yield a + b + c

    result.value
  }

  //type Response[A] = Future[Either[String, A]]
  type Response[A] = EitherT[Future, String, A]

  import cats.syntax.either._
  val powerLevels = Map ("Jazz" -> 6, "Bumblebee" -> 8, "Hot Rod" -> 10)

  def getPowerLevel(autobot: String): Response[Int] = powerLevels.get(autobot) match {
    case Some(a) => EitherT(Future(a.asRight[String]))
    case None => EitherT(Future(s"can't get power for $autobot".asLeft[Int]))
  }

  def canSpecialMove(ally1: String, ally2: String): Response[Boolean] = for {
    a <- getPowerLevel(ally1)
    b <- getPowerLevel(ally2)
  } yield a + b > 15

  def tacticalReport(ally1: String, ally2: String): String = {
    val f = canSpecialMove(ally1, ally2).value
    Await.result(f, 1.second) match {
      case Right(b) if b => s"$ally1 and $ally2 can move"
      case _ => s"$ally1 and $ally2 can't move"
    }
  }
  println(tacticalReport("Jazz", "Bumblebee"))

}