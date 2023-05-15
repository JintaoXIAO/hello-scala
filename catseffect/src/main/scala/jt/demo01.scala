package demo01

import cats.effect.{ExitCode, IO, IOApp, IOLocal}

import scala.concurrent.TimeoutException



object Main02 extends IOApp {

  def inc(name: String, local: IOLocal[Int]): IO[Unit] =
    local.update(_ + 1) >> local.get.flatMap{ a => IO.println(s"fiber $name $a")}

  def update(name: String, local: IOLocal[Int], f: Int => Int): IO[Unit] =
    local.update(f) >> local.get.flatMap(c => IO.println(s"$name: $c"))

  override def run(args: List[String]): IO[ExitCode] =
    for {
      local <- IOLocal(43)
      fib1 <- update("fiber B", local, _ - 1).start
      fib2 <- update("fiber C", local, _ + 1).start
      _ <- fib1.joinWithNever
      _ <- fib2.joinWithNever
      c <- local.get
      _ <- IO.println(s"fiber A: $c")

    } yield ExitCode.Success
}


object Main01 extends IOApp {
  import cats.effect.IO
  import scala.concurrent.duration._

  val ioa = IO.println("hello world")

  override def run(args: List[String]): IO[ExitCode] = {
    IO.println("hello world").andWait(1.second).foreverM.timeout(3.second)
  }
}
