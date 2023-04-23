package rockjvm.part2

import cats.effect.IO

import scala.io.StdIn




object IOIntro {

  val ourFirstIO: IO[Int] = IO.pure(32)
  val aDelayedIO: IO[Int] = IO.delay {
    println("producing an integer")
    123
  }

  val aDelayedIO_V2: IO[Int] = IO {
    println("producing an integer")
    123
  }

  val improvedMeaningOfLife: IO[Int] = ourFirstIO.map(_ * 2)
  val printedMeaningOfLife = ourFirstIO.flatMap(mol => IO.delay(println(mol)))

  import scala.io.StdIn
  def smallProg(): IO[Unit] = for {
    l1 <- IO(StdIn.readLine())
    l2 <- IO(StdIn.readLine())
    _ <- IO.delay(println(l1 + l2))
  } yield ()

  import cats.syntax.apply._
  val combinedMeaningOfLife = (ourFirstIO, improvedMeaningOfLife).mapN(_ + _)

  def smallProg2(): IO[Unit] =
    (IO(StdIn.readLine()), IO(StdIn.readLine())).mapN(_ + _).map(_.toUpperCase).map(println)

  def sequenceTakeLast[A,B](ioa: IO[A], iob: IO[B]): IO[B] =
    ioa.flatMap(_ => iob)

  def sequenceTakeLast_v2[A,B](ioa: IO[A], iob: IO[B]): IO[B] =
    ioa *> iob

  def sequenceTakeLast_v3[A, B](ioa: IO[A], iob: IO[B]): IO[B] =
    ioa >> iob

  def sequenceTaskFirst[A,B](ioa: IO[A], iob: IO[B]): IO[A] =
    ioa.flatMap(a => iob.map(_ => a))

  def sequenceTaskFirst_v2[A, B](ioa: IO[A], iob: IO[B]): IO[A] =
    ioa <* iob

  def forever[A](io: IO[A]): IO[A] =
    io.flatMap(_ => forever(io))

  def convert[A, B](ioa: IO[A], value: B): IO[B] =
    ioa.map(_ => value)

  def convert_v2[A, B](ioa: IO[A], value: B): IO[B] =
    ioa.as(value)

  def asUnit[A](ioa: IO[A]): IO[Unit] =
    ioa.map(_ => ())

  def asUnit_v2[A](ioa: IO[A]): IO[Unit] =
    ioa.void

  // stack unsafe
  def sum(n: Int): Int =
    if (n <= 0) 0
    else n + sum(n - 1)

  def sumIO(n: Int): IO[Int] =
    if (n <= 0) IO(0)
    else for {
      a <- IO(n)
      b <- sumIO(n - 1)
    } yield a + b

  def fib(n: Int): Int =
    if (n < 2)
      1
    else
      fib(n-1) + fib(n-2)

  def fibonacci(n: Int): IO[BigInt] =
    if (n < 2)
      IO(1)
    else for {
      a <- IO(fibonacci(n-1)).flatten
      b <- IO.defer(fibonacci(n-2))
    } yield a + b

  def main(args: Array[String]): Unit = {
    import cats.effect.unsafe.implicits.global
    //println(smallProg2().unsafeRunSync())
    //println(sum(10000000))
    //println(sumIO(100).unsafeRunSync())
    println(fib(10))
  }

}
