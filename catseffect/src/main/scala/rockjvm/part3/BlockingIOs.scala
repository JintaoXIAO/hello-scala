package rockjvm.part3

import cats.effect.{IO, IOApp}

import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.DurationInt

object BlockingIOs extends IOApp.Simple {

  import rockjvm.utils._

  val someSleeps: IO[Unit] = for {
    _ <- IO.sleep(1.second).myDebug
    _ <- IO.sleep(1.second).myDebug
  } yield ()

  val aBlockingIO: IO[Int] = IO.blocking {
    Thread.sleep(1000)
    println(s"[${Thread.currentThread().getName}] computed a blocking code")
    43
  }

  val iosOnManyThreads: IO[Unit] = for {
    _ <- IO("first").myDebug
    _ <- IO.cede
    _ <- IO("second").myDebug
    _ <- IO.cede
    _ <- IO("third").myDebug
  } yield ()

  def testThousandEffectsSwitch(): IO[Int] = {
    val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
    (1 to 1000).map(IO.pure).reduce(_.myDebug >> IO.cede >> _.myDebug).evalOn(ec)
  }

  override def run: IO[Unit] = testThousandEffectsSwitch().void
}
