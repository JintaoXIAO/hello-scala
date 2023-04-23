package rockjvm.part2

import cats.Parallel
import cats.effect.{IO, IOApp}

object IOParallelism extends IOApp.Simple {

  val anisIO = IO(s"[${Thread.currentThread().getName}] Ani")
  val kamranIO = IO(s"[${Thread.currentThread().getName}] Kamran")

  val composedIO = for {
    ani <- anisIO
    kamran <- kamranIO
  } yield s"$ani and $kamran love Rock the JVM"

  import rockjvm.utils._
  val meaningOfLife: IO[Int] = IO.delay(42).myDebug
  val favLanguage: IO[String] = IO.delay("Scala").myDebug

  import cats.syntax.apply._
  val goalInLife: IO[String] = (meaningOfLife, favLanguage).mapN((num, str) => s"My goal in life is $num and $str")

  val parIO1: IO.Par[Int] = Parallel[IO].parallel(meaningOfLife)
  val parIO2: IO.Par[String] = Parallel[IO].parallel(favLanguage)

  import cats.effect.implicits._
  val goalInLifeParallel: IO.Par[String] = (parIO1, parIO2).mapN((num, str) => s"my goal in life is $num and $str")

  val goalInLife_v2 = Parallel[IO].sequential(goalInLifeParallel)

  import cats.syntax.parallel._
  val goalInLife_v3: IO[String] = (meaningOfLife, favLanguage).parMapN((num, str) => s"my goal in life is $num and $str")

  val aFailure: IO[String] = IO.raiseError(new RuntimeException("I can't do this"))
  val parallelWithFailure = (meaningOfLife, aFailure.myDebug).parMapN(_ + _)
  val anotherFailure: IO[String] = IO.raiseError(new RuntimeException("Second failure"))
  val twoFailures: IO[String] = (aFailure.myDebug, anotherFailure.myDebug).parMapN(_ + _)
  val twoFailuresDelayed: IO[String] = (IO(Thread.sleep(1000)) >> aFailure.myDebug, anotherFailure.myDebug).parMapN(_ + _)


  override def run: IO[Unit] =
    twoFailuresDelayed.myDebug.void
    // twoFailures.myDebug.void
    // parallelWithFailure.myDebug.map(println)
    // goalInLife_v3.myDebug.map(println)
    //goalInLife_v2.myDebug.map(println)
    //composedIO.map(println)
    // goalInLife.map(println)


}
