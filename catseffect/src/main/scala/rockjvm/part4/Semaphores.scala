package rockjvm.part4

import cats.effect.std.Semaphore
import cats.effect.{IO, IOApp}
import cats.syntax.parallel._
import rockjvm.utils._

import scala.concurrent.duration.DurationInt
import scala.util.Random

object Semaphores extends IOApp.Simple {

  val semaphore: IO[Semaphore[IO]] = Semaphore[IO](2) // 2 permits

  def doWorkWhileLoggedIn(): IO[Int] = IO.sleep(1.second) >> IO(Random.nextInt(100))

  def login(id: Int, sem: Semaphore[IO]): IO[Int] = for {
    _ <- IO(s"[session $id] waiting to log in...").myDebug
    _ <- sem.acquire
    _ <- IO(s"[session $id] logged in, working...").myDebug
    res <- doWorkWhileLoggedIn()
    _ <- IO(s"[session $id] done: $res, logging out...").myDebug
    _ <- sem.release
  } yield res

  def demoSemaphore() = for {
    sem <- Semaphore[IO](2)
    user1Fib <- login(1, sem).start
    user2Fib <- login(2, sem).start
    user3Fib <- login(3, sem).start

    _ <- user1Fib.join
    _ <- user2Fib.join
    _ <- user3Fib.join
  } yield ()

  def weightedLogin(id: Int, requiredPermits: Int, sem: Semaphore[IO]): IO[Int] = for {
    _ <- IO(s"[session $id] waiting to log in...").myDebug
    _ <- sem.acquireN(requiredPermits)
    _ <- IO(s"[session $id] logged in, working...").myDebug
    res <- doWorkWhileLoggedIn()
    _ <- IO(s"[session $id] done: $res, logging out...").myDebug
    _ <- sem.release
  } yield res

  def demoWeightedSemaphore() = for {
    sem <- Semaphore[IO](2)
    user1Fib <- weightedLogin(1, 1, sem).start
    user2Fib <- weightedLogin(2, 2, sem).start
    user3Fib <- weightedLogin(3, 3, sem).start

    _ <- user1Fib.join
    _ <- user2Fib.join
    _ <- user3Fib.join
  } yield ()

  val mutex = Semaphore[IO](1)
  def users(sem: Semaphore[IO]): IO[List[Int]] = (1 to 10).toList.parTraverse{ id =>
    for {
      _ <- IO(s"[session $id] waiting to log in...").myDebug
      _ <- sem.acquire
      _ <- IO(s"[session $id] logged in, working...").myDebug
      res <- doWorkWhileLoggedIn()
      _ <- IO(s"[session $id] done: $res, logging out...").myDebug
      _ <- sem.release
    } yield res
  }

  override def run: IO[Unit] = for {
    sem <- mutex
    _ <- users(sem)
  } yield ()
}
