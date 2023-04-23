package rockjvm.part3

import cats.effect.{IO, IOApp}

import java.util.concurrent.{ExecutorService, Executors}
import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService, Future}
import scala.util.Try
import rockjvm.utils._

import scala.concurrent.duration.DurationInt

object AsyncIOs extends IOApp.Simple {

  val threadPool: ExecutorService = Executors.newFixedThreadPool(8)
  val ec: ExecutionContextExecutorService = ExecutionContext.fromExecutorService(threadPool)

  type Callback[A] = Either[Throwable, A] => Unit
  def computeMeaningOfLie(): Int = {
    Thread.sleep(1000)
    println(s"[${Thread.currentThread().getName}] computing meaningOfLife on some other thread...")
    32
  }

  def computeMeaningOfLifeEither(): Either[Throwable, Int] = Try {
    Thread.sleep(1000)
    println(s"[${Thread.currentThread().getName}] computing meaningOfLife on some other thread...")
    32
  }.toEither

  def computeMolOnThreadPool(): Unit =
    threadPool.execute(() => computeMeaningOfLifeEither())

  def asyncMolIO: IO[Int] = IO.async_ { cb =>
    threadPool.execute{ () =>
      val rst = computeMeaningOfLifeEither()
      cb(rst)
    }
  }

  def asyncToIO[A](computation: () => A)(ec: ExecutionContext): IO[A] =
    IO.async_[A] { (cb: Callback[A]) =>
      ec.execute { () =>
        val result = Try(computation()).toEither
        cb(result)
      }
    }

  val asyncMolIO_v2 = asyncToIO(computeMeaningOfLie)(ec)

  lazy val molFuture: Future[Int] = Future{ computeMeaningOfLie() }(ec)

  def convertFutureToIO[A](future: => Future[A]): IO[A] =
    IO.async_{ cb =>
      future.onComplete { tryResult =>
        val result = tryResult.toEither
        cb(result)
      }(ec)
    }

  val asyncMolIO_V3 = convertFutureToIO(molFuture)
  val asyncMolIO_v4 = IO.fromFuture(IO(molFuture))

  val neverEndingIO: IO[Int] = IO.async_(_ => ())
  val neverEndingIO2: IO[Int] = IO.never[Int]

  def demoAsyncCancellation = {
    val asyncMeaningOfLifeIO_v2: IO[Int] = IO.async { (cb: Callback[Int]) =>
      IO {
        threadPool.execute { () =>
          val result = computeMeaningOfLifeEither()
          cb(result)
        }
      }.as(Some(IO("Cancelled").myDebug.void))

    }

    for {
      fib <- asyncMeaningOfLifeIO_v2.start
      _ <- IO.sleep(500.millis) >> IO("cancenlling").myDebug >> fib.cancel
      _ <- fib.join
    } yield ()
  }

  override def run: IO[Unit] = asyncMolIO_v2.myDebug >> IO(threadPool.shutdownNow())
}
