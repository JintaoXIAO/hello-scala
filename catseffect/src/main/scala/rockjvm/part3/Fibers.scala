package rockjvm.part3

import cats.effect.kernel.Outcome
import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}
import cats.effect.{Fiber, FiberIO, IO, IOApp}
import cats.implicits.catsSyntaxTuple2Semigroupal

import scala.concurrent.duration.{DurationInt, FiniteDuration}

object Fibers extends IOApp.Simple{

  val meaningOfLife: IO[Int] = IO(42)
  val favLang: IO[String] = IO.pure("Scala")
  import rockjvm.utils._

  def sameThreadIOs(): IO[Unit] = for {
    mol <- meaningOfLife.myDebug
    lang <- favLang.myDebug
  } yield ()

  def createFiber: Fiber[IO, Throwable, String] = ???

  // type FiberIO[A] = Fiber[IO, Throwable, A]
  val aFiber: IO[FiberIO[Int]] = meaningOfLife.myDebug.start

  def differentThreadIOs(): IO[Unit] = for {
    _ <- aFiber
    _ <- favLang.myDebug
  } yield ()

  def runOnSomeOtherThread[A](io: IO[A]): IO[Outcome[IO, Throwable, A]] = for {
    fib <- io.start
    rst <- fib.join
  } yield rst

  val someIOOnAnotherThread = runOnSomeOtherThread(meaningOfLife)
  val someResultFromAnotherThread: IO[Int] = someIOOnAnotherThread.flatMap {
    case Succeeded(effect) => effect
    case Errored(_) => IO(0)
    case Canceled() => IO(0)
  }

  def throwOnAnotherThread(): IO[Outcome[IO, Throwable, Int]] = for {
    fib <- IO.raiseError[Int](new RuntimeException("no number for you")).start
    result <- fib.join
  } yield result

  def testCancel(): IO[Outcome[IO, Throwable, String]] = {
    val task = IO("starting").myDebug >> IO.sleep(1.second) >> IO("done").myDebug
    val taskWithCancellationHandler = task.onCancel(IO("I'm being cancelled").myDebug.void)

    for {
      fib <- taskWithCancellationHandler.start
      _ <- IO.sleep(500.millis) >> IO("cancelling").myDebug
      _ <- fib.cancel
      rst <- fib.join
    } yield rst
  }

  def processResultsFromFiber[A](io: IO[A]): IO[A] = (for {
    fib <- io.start.myDebug
    rst <- fib.join
  } yield rst).flatMap {
    case Succeeded(eff) => eff
    case _ => IO.raiseError(new RuntimeException("errored or canceled"))
  }

  def tupleIOs[A,B](ioa: IO[A], iob: IO[B]): IO[(A, B)] = (for {
    fiba <- ioa.start.myDebug
    fibb <- iob.start.myDebug

    rst1 <- fiba.join
    rst2 <- fibb.join
  } yield (rst1, rst2)).flatMap {
    case (Succeeded(eff1), Succeeded(eff2)) => (eff1, eff2).mapN((_, _))
    case (Errored(err), _) => IO.raiseError(err)
    case (_, Errored(err)) => IO.raiseError(err)
    case (Canceled(), _) => IO.raiseError(new RuntimeException("the first cancelled"))
    case (_, Canceled()) => IO.raiseError(new RuntimeException("the second cancelled"))
  }

  def timeout[A](io: IO[A], duration: FiniteDuration): IO[A] = (for {
    fib <- io.timeout(duration).start
    rst <- fib.join
  } yield rst).flatMap {
    case Succeeded(eff) => eff
    case Errored(err) => IO.raiseError(err)
    case Canceled() => IO.raiseError(new RuntimeException("timeout"))
  }

  override def run: IO[Unit] =
    testCancel().myDebug.void
}
