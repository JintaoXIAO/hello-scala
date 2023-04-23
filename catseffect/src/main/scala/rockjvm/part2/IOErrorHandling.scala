package rockjvm.part2

import cats.effect.IO
import cats.effect.unsafe.implicits.global

import scala.util.{ Try, Success, Failure }

object IOErrorHandling {
  val aFailedCompute: IO[Int] = IO.delay(throw new RuntimeException("A FAILURE"))
  val aFailure: IO[Int] = IO.raiseError(new RuntimeException("a proper failure"))

  val dealWithIt = aFailure.handleErrorWith {
    case _: RuntimeException => IO.delay(println("I'm still here"))
  }

  val effectAsEither: IO[Either[Throwable, Int]] = aFailure.attempt

  val resultAsString = aFailure.redeem(ex => s"FAIL $ex", value => s"SUCCESS: $value")

  val resultAsEffect = aFailure.redeem(ex => IO.println(s"FAIL: $ex"), value => IO(println(s"SUCCESS $value")))

  def option2IO[A](option: Option[A])(ifEmpty: Throwable): IO[A] = option match {
    case None => IO.raiseError(ifEmpty)
    case Some(a) => IO(a)
  }

  def try2IO[A](aTry: Try[A]): IO[A] = aTry match {
    case Failure(exception) => IO.raiseError(exception)
    case Success(value) => IO(value)
  }

  def either2IO[A](anEither: Either[Throwable, A]): IO[A] = anEither match {
    case Left(thr) => IO.raiseError(thr)
    case Right(value) => IO(value)
  }

  def handleIOError[A](io: IO[A])(handler: Throwable => A): IO[A] =
    // io.handleError(handler)
    io.redeem(handler, identity)

  def handleIOErrorWith[A](io: IO[A])(handler: Throwable => IO[A]): IO[A] =
    //io.handleErrorWith(handler)
    io.redeemWith(handler, IO.pure)

  def main(args: Array[String]): Unit = {
    //aFailedCompute.unsafeRunSync()
    println(resultAsString.unsafeRunSync())
  }
}
