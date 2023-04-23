package rockjvm.part2

import cats.effect.{IO, IOApp}

import scala.concurrent.Future
import scala.util.Random

object IOTraversal extends IOApp.Simple{

  import scala.concurrent.ExecutionContext.Implicits.global
  def heavyComputation(str: String): Future[Int] = Future {
    Thread.sleep(Random.nextInt(1000))
    str.split(" ").length
  }

  val workload: List[String] = List("I quite like CE", "scala is great", "looking forward to some awesome stuff")
  def clunkyFutures(): Unit = {
    val futures: List[Future[Int]] = workload.map(heavyComputation)
    futures.foreach(_.foreach(println))
  }

  import cats.Traverse
  import cats.instances.list._

  val listTraverse = Traverse[List]

  def traverseFutures(): Unit = {

    val singleFuture: Future[List[Int]] = listTraverse.traverse(workload)(heavyComputation)

    singleFuture.foreach(println)
  }

  import rockjvm.utils._
  import cats.effect.IO
  def computeAsIO(str: String): IO[Int] = IO {
    Thread.sleep(Random.nextInt(1000))
    str.split(" ").length
  }.myDebug

  val ios: List[IO[Int]] = workload.map(computeAsIO)
  val singleIO: IO[List[Int]] = listTraverse.traverse(workload)(computeAsIO)

  import cats.syntax.parallel._
  import cats.syntax.traverse._
  val parallelSingleIO: IO[List[Int]] = workload.parTraverse(computeAsIO)

  def sequence[A](listOfIOs: List[IO[A]]): IO[List[A]] = listOfIOs.traverse(identity)

  def sequence2[F[_]: Traverse, A](listOfIOs: F[IO[A]]): IO[F[A]] = listOfIOs.traverse(identity)

  def parSequence[A](listOfIOs: List[IO[A]]): IO[List[A]] = listOfIOs.parTraverse(identity)

  def parSequence2[F[_]: Traverse, A](listOfIOs: F[IO[A]]): IO[F[A]] = listOfIOs.parTraverse(identity)

  override def run: IO[Unit] =
    parallelSingleIO.map(_.sum).myDebug.void
    //singleIO.map(_.sum).myDebug.void
}
