package rockjvm.part3

import cats.effect.{FiberIO, IO, IOApp, OutcomeIO}

import cats.effect.kernel.Outcome.{ Succeeded, Errored, Canceled}

import scala.concurrent.duration.{DurationInt, FiniteDuration}

object RacingIOs extends IOApp.Simple {

  import rockjvm.utils._

  def runWithSleep[A](value: A, duration: FiniteDuration): IO[A] =
    (IO(s"starting computation").myDebug >>
      IO.sleep(duration) >>
      IO(s"computation done") >>
      IO(value)
      ).onCancel(IO(s"computation CANCELLED for $value").myDebug.void)

  def testRace()= {
    val meaningOfLife: IO[Int] = runWithSleep(42, 1.second)
    val favLang: IO[String] = runWithSleep("scala", 2.second)

    val first: IO[Either[Int, String]] = IO.race(meaningOfLife, favLang)
    first.flatMap {
      case Left(a) => IO(s"Meaing of live won: $a")
      case Right(s) => IO(s"Fav lang won: $s")
    }
  }

  def testRacePair() = {
    val meaningOfLife: IO[Int] = runWithSleep(42, 1.second)
    val favLang: IO[String] = runWithSleep("scala", 2.second)
    val raceResult: IO[Either[(OutcomeIO[Int], FiberIO[String]), (FiberIO[Int], OutcomeIO[String])]] =
      IO.racePair(meaningOfLife, favLang)

    raceResult.flatMap{
      case Left((outM, fibL)) => fibL.cancel >> IO("MOL won").myDebug >> IO(outM).myDebug
      case Right((fibM, outL)) => fibM.cancel >> IO("Language won").myDebug >> IO(outL).myDebug
    }
  }

  def timeout[A](io: IO[A], duration: FiniteDuration): IO[A] = {
    IO.race(io, IO.sleep(duration)).flatMap {
      case Left(a) => IO(a)
      case _ => IO.raiseError(new RuntimeException(s"timeout after $duration"))
    }
  }

  def unrace[A,B](ioa: IO[A], iob: IO[B]): IO[Either[A, B]] =
    IO.racePair(ioa, iob).flatMap {
      case Left((_, fibB)) => fibB.join.flatMap {
        case Succeeded(fb) => fb.map(Right(_))
        case _ => IO.raiseError(new RuntimeException("cancel or failed"))
      }
      case Right((fibA, _)) => fibA.join.flatMap {
        case Succeeded(fa) => fa.map(Left(_))
        case _ => IO.raiseError(new RuntimeException("cancel or failed"))
      }
    }

  import cats.syntax.either._
  def simpleRace[A,B](ioa: IO[A], iob: IO[B]): IO[Either[A, B]] =
    IO.racePair(ioa, iob).flatMap{
      case Left((oa, _)) => oa match {
        case Succeeded(fa) => fa.flatMap(a => IO(a.asLeft[B]))
        case _ => IO.raiseError(new RuntimeException("cancel or failed"))
      }
      case Right((_, ob)) => ob match {
        case Succeeded(fb) => fb.flatMap(b => IO(b.asRight[A]))
        case _ => IO.raiseError(new RuntimeException("cancel or failed"))
      }
    }

  override def run: IO[Unit] = testRacePair().myDebug.void
}
