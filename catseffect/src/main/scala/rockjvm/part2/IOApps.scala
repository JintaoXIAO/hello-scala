package rockjvm.part2

import cats.effect.{ExitCode, IO, IOApp}

import scala.io.StdIn

object IOApps {
  val program = for {
    line <- IO(StdIn.readLine())
    _ <- IO.println(s"You've just writen: $line")
  } yield ()

}

object FirstCatsEffectApp extends IOApp {
  import IOApps._
  override def run(args: List[String]): IO[ExitCode] =
    program.map(_ => ExitCode.Success)

}

object MySimpleApp extends IOApp.Simple {
  import IOApps._
  override def run: IO[Unit] = program
}