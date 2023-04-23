package rockjvm.part4

import cats.effect.{IO, IOApp, Ref}
import cats.implicits._
import rockjvm.utils._

import scala.concurrent.duration.DurationInt

object Refs extends IOApp.Simple {
  val atomicMol: IO[Ref[IO, Int]] = Ref[IO].of(42)
  val atomicMol_v2: IO[Ref[IO, Int]] = IO.ref(42)

  val increasedMol: IO[Unit] = atomicMol.flatMap { ref =>
    ref.set(43)
  }

  val mol: IO[Int] = atomicMol.flatMap{ ref =>
    ref.get
  }

  val gsMol = atomicMol.flatMap { ref =>
    val old = ref.getAndSet(43)
    old
  }

  val fMol: IO[Int] = atomicMol.flatMap { ref =>
    ref.updateAndGet(_ + 1)
  }

  val modifiedMol: IO[String] = atomicMol.flatMap{ ref =>
    ref.modify(i => (i * 10, s"current value is : $i"))
  }

  def demoConcurrentWorkImpure(): IO[Unit] = {
    import cats.syntax.parallel._
    var count: Int = 0
    def task(workload: String): IO[Unit] = {
      val workCount = workload.split(" ").length
      for {
        _ <- IO(s"Counting words for '$workload': $workCount").myDebug
        newCount <- IO(count + workCount)
        _ <- IO(s"new total: $newCount").myDebug
        _ <- IO(count += newCount)
      } yield ()
    }

    List("I love cats effect", "this ref thing is useless", "hello world from jt")
      .map(task)
      .parSequence
      .void
  }

  def demoConcurentWorkPure(): IO[Unit] = {
    import cats.syntax.parallel._
    def task(workload: String, total: Ref[IO, Int]): IO[Unit] = {
      val workCount = workload.split(" ").length
      for {
        _ <- IO(s"Counting words for '$workload': $workCount").myDebug
        newCount <- total.updateAndGet(_ + workCount)
        _ <- IO(s"new total: $newCount").myDebug
      } yield ()
    }
    for {
      initialCount <- Ref[IO].of(0)
      _ <- List ("I love cats effect", "this ref thing is useless", "hello world from jt")
        .map(s => task(s, initialCount))
        .parSequence
    } yield ()
  }

  def tickingClockImpure(): IO[Unit] = {
    var ticks: Long = 0L
    def tickingClock: IO[Unit] = for {
      _ <- IO.sleep(1.second)
      _ <- IO(System.currentTimeMillis()).myDebug
      _ <- IO(ticks += 1)
      _ <- tickingClock
    } yield ()

    def printTicks: IO[Unit] = for {
      _ <- IO.sleep(5.seconds)
      _ <- IO(s"TICKS: $ticks").myDebug
      _ <- printTicks
    } yield ()

    for {
      _ <- (tickingClock, printTicks).parTupled
    } yield ()
  }

  def tickingClockPure(): IO[Unit] = {
    def tickingClock(ticks: Ref[IO, Long]): IO[Unit] = for {
      _ <- IO.sleep(100.millis)
      _ <- IO(System.currentTimeMillis()).myDebug
      _ <- ticks.update(_ + 1)
      _ <- tickingClock(ticks)
    } yield ()

    def printTicks(ticks: Ref[IO, Long]): IO[Unit] = for {
      _ <- IO.sleep(5.seconds)
      tick <- ticks.get
      _ <- IO(s"TICKS: $tick").myDebug
      _ <- printTicks(ticks)
    } yield ()

    for {
      ref <- IO.ref[Long](0L)
      _ <- (tickingClock(ref), printTicks(ref)).parTupled
    } yield ()
  }

  def tickingClockWeird(): IO[Unit] = {
    val ticks: IO[Ref[IO, Long]] =  IO.ref[Long](0L)

    def tickingClock: IO[Unit] = for {
      t <- ticks
      _ <- IO.sleep(1.second)
      _ <- IO(System.currentTimeMillis()).myDebug
      _ <- t.update(_ + 1)
      _ <- tickingClock
    } yield ()

    def printTicks: IO[Unit] = for {
      t <- ticks
      _ <- IO.sleep(5.seconds)
      c <- t.get
      _ <- IO(s"TICKS: ${c}").myDebug
      _ <- printTicks
    } yield ()

    for {
      _ <- (tickingClock, printTicks).parTupled
    } yield ()
  }


  override def run: IO[Unit] = tickingClockWeird
}