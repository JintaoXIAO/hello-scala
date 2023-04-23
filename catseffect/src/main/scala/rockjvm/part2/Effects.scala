package rockjvm.part2

import scala.io.StdIn

object Effects {

  case class MyIO[A](unsafeRun: () => A) {
    def map[B](f: A => B): MyIO[B] = MyIO[B](() => f(unsafeRun()))
    def flatMap[B](f: A => MyIO[B]): MyIO[B] = MyIO(() => f(unsafeRun()).unsafeRun())
  }

  val anIO: MyIO[Int] = MyIO(() => {
    println("I'm writing something...")
    12
  })

  def clock: MyIO[Long] = MyIO(() => System.currentTimeMillis())

  def measure[A](computation: MyIO[A]): MyIO[Long] = for {
    s <- clock
    _ <- computation
    e <- clock
  } yield e - s

  def testTimeIO(): Unit = {
    val test = measure(MyIO(() => Thread.sleep(1000)))
    println(test.unsafeRun())
  }

  def putStrLn(line: String): MyIO[Unit] = MyIO(() => println(line))

  def read: MyIO[String] = MyIO(() => StdIn.readLine())

  def testConsole(): Unit = {
    var program = for {
      line1 <- read
      line2 <- read
      _ <- putStrLn(line1 + line2)
    } yield ()
    program.unsafeRun()
  }

  def main(args: Array[String]): Unit = {
    //testTimeIO()
    testConsole()
  }

}
