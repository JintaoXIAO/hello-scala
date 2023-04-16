package monad

import java.util.logging.Logger

object Main extends App {

  import cats.Monad
  import cats.instances.option._
  import cats.instances.list._

  val opt1: Option[Int] = Monad[Option].pure(3)
  val opt2: Option[Int] = Monad[Option].flatMap(opt1)(a => Some(a + 2))
  val opt3: Option[Int] = Monad[Option].map(opt2)(a => a * 100)

  val list1: List[Int] = Monad[List].pure(3)

  def parseInt(str: String): Option[Int] = scala.util.Try(str.toInt).toOption

  def divide(a: Int, b: Int): Option[Int] =
    if (b == 0) None else Some(a / b)

  def stringDivideBy(aStr: String, bStr: String): Option[Int] =
    parseInt(aStr).flatMap { a =>
      parseInt(bStr).flatMap { b => divide(a, b) }
    }

  import cats.instances.option._
  import cats.syntax.flatMap._
  import cats.syntax.applicative._
  import cats.syntax.functor._

  1.pure[Option]
  1.pure[List]

  def sumSquare[F[_] : Monad](a: F[Int], b: F[Int]): F[Int] = for {
    x <- a
    y <- b
  } yield x * x + y * y

  println(sumSquare(List(1, 2, 3), List(3, 4, 5)))

  import cats.Id

  println(sumSquare(Id(1), Id(2)))

  val either1: Either[String, Int] = Right(10)
  val either2: Either[String, Int] = Right(20)

  println(for {
    a <- either1
    b <- either2
  } yield a + b)

  import cats.syntax.either._

  val a = 3.asRight[String]
  val b = 4.asRight[String]

  def countPositive(nums: List[Int]) =
    nums.foldLeft(0.asRight[String]) { (acc, num) =>
      if (num < 0)
        acc.map(_ + 1)
      else
        "Negative. Stopping".asLeft[Int]
    }

  println(countPositive(List(1, 2, 3)))
  println(countPositive(List(1, -2, 3)))

  type Result[A] = Either[Throwable, A]

  object Wrapper {
    sealed trait LoginError extends Product with Serializable

    final case class UserNotFound(username: String) extends LoginError

    final case class PasswordIncorrect(username: String) extends LoginError

    case object UnexpectedError extends LoginError
  }

  import Wrapper._

  case class User(username: String, password: String)

  type LoginResult = Either[LoginError, User]

  def handleError(error: LoginError): Unit = error match {
    case UserNotFound(u) => println(s"User not found: $u")
    case PasswordIncorrect(u) => println(s"Password incorrect: $u")
    case UnexpectedError => println("Unexpected error")
  }

  val reulst1: LoginResult = User("dave", "pwd").asRight[LoginError]
  val result2: LoginResult = UserNotFound("dave").asLeft

  import cats.MonadError
  import cats.instances.either._

  type ErrorOr[A] = Either[String, A]

  val monadError = MonadError[ErrorOr, String]
  val success = monadError.pure(12)
  val failure = monadError.raiseError("Badness")
  println(success)
  println(failure)

  println(monadError.handleErrorWith(success) {
    case "Badness" => monadError.pure(12)
    case _ => monadError.raiseError("It's not ok")
  })

  println(monadError.handleError(failure) {
    case "Badness" => 13
    case _ => -1
  })

  println(monadError.ensure(success)("Number too low!")(_ > 1000))

  import scala.util.Try

  def validateAdult[F[_]](age: Int)(implicit me: MonadError[F, Throwable]): F[Int] =
    if (age >= 18)
      me.pure(18)
    else
      me.raiseError(new IllegalArgumentException())

  println(validateAdult[Try](18))
  println(validateAdult[Try](8))

  type ExceptionOr[A] = Either[Throwable, A]
  println(validateAdult[ExceptionOr](-1))


  println(">>>>>>>>>>>>>> Eval monad")

  val x = {
    println("computing x")
    math.random
  }

  def y = {
    println("Computing y")
    math.random
  }

  import cats.Eval

  val now = Eval.now(math.random + 1000)
  val always = Eval.always(math.random + 3000)
  val later = Eval.later(math.random + 2000)

  def factorial(n: BigInt): Eval[BigInt] =
    if (n == 1)
      Eval.now(n)
    else {
      Eval.defer(factorial(n - 1).map(_ * n))
    }

  def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B = as match {
    case h :: tl => fn(h, foldRight(tl, acc)(fn))
    case Nil => acc
  }

  def foldRight1[A, B](as: List[A], acc: B)(fn: (A, Eval[B]) => Eval[B]): Eval[B] = as match {
    case h :: tl => fn(h, Eval.defer(foldRight1(tl, acc)(fn)))
    case Nil => Eval.now(acc)
  }

  println(">>>>>>>>>>>>>>>> Writer monad")

  import cats.data.Writer
  import cats.instances.vector._
  import cats.syntax.applicative._
  import cats.syntax.writer._

  type Logged[A] = Writer[Vector[String], A]

  //println(123.pure[Logged])
  //println(Vector("hello", "world").tell)

  val a1 = Writer(Vector("msg1", "msg2"), 123)
  val b1 = 123.writer(Vector("msg1", "msg2"))
  //println(a1)
  //println(b1)

  val writer1 = for {
    a <- 10.pure[Logged]
    _ <- Vector("a", "b", "c").tell
    b <- 123.writer(Vector("x", "y", "z"))
  } yield a + b

  // val (logcontext, result) = writer1.run
  //println(s"rst: $result, log: $logcontext")
  val writer2 = writer1.mapWritten(_.map(_.toUpperCase))
  val writer3 = writer1.bimap(
    log => log.map(_.toUpperCase),
    res => res * 100
  )
  val writer4 = writer1.mapBoth { (log, rst) =>
    val log2 = log.map(_ + "!")
    val rst2 = rst * 12
    (log2, rst2)
  }

  def slowly[A](body: => A) = {
    try body
    finally Thread.sleep(100)
  }

  def factorial(n: Int): Int = {
    val ans = slowly(if (n == 0) 1 else n * factorial(n - 1))
    println(s"fact $n $ans")
    ans
  }

  def factorial1(n: Int): Logged[Int] = for {
    ans <- slowly(if (n == 0) 1.pure[Logged] else factorial1(n - 1).map(_ * n))
    _ <- Vector(s"fact $n $ans").tell
  } yield ans

  import scala.concurrent._
  import scala.concurrent.ExecutionContext.Implicits._
  import scala.concurrent.duration._

  /*
  val result: Vector[Logged[Int]] = Await.result(Future.sequence(Vector(
    Future(factorial1(5)),
    Future(factorial1(5))
  )), 5.second)

  result.foreach{ l =>
    val log = l.written
    println(log)
  }
*/
  println(">>>>>>>>>>>>> Reader monad")

  import cats.data.Reader

  final case class Cat(name: String, favoriteFood: String)

  val catName: Reader[Cat, String] = Reader(c => c.name)
  val greetKitty = catName.map(name => s"hello $name")
  val feedKitty: Reader[Cat, String] = Reader(c => s"Have a nice bowl of ${c.favoriteFood}")
  val greetAndFeed: Reader[Cat, String] = for {
    greet <- greetKitty
    feed <- feedKitty
  } yield s"$greet. $feed"

  println(greetAndFeed(Cat("Garfield", "lasagne")))

  final case class Db(usernames: Map[Int, String]
                      , passwords: Map[String, String])

  type DbReader[A] = Reader[Db, A]

  def findUsername(userId: Int): DbReader[Option[String]] =
    Reader(db => db.usernames.get(userId))

  def checkPassword(username: String, password: String): DbReader[Boolean] =
    Reader(db => db.passwords.get(username) match {
      case None => false
      case Some(p) => p == password
    })

  def checkLogin(userId: Int, password: String): DbReader[Boolean] =
    findUsername(userId).flatMap {
      case None => Reader(_ => false)
      case Some(u) => checkPassword(u, password)
    }

  val users = Map(
    1 -> "dade",
    2 -> "kate",
    3 -> "margo"
  )
  val passwords = Map(
    "dade" -> "zerocool",
    "kate" -> "acidburn",
    "margo" -> "secret"
  )

  val db = Db(users, passwords)
  println(checkLogin(1, "zerocool").run(db))

  println(">>>>>>>>>>>> State monad")
  import cats.data.State
  val step1 = State[Int, String] { a =>
    (a + 1, s"Result of step1: $a ")
  }
  val step2 = State[Int, String] { a =>
    (a * 2, s"Result of step2: $a ")
  }
  val both = for {
    a <- step1
    b <- step2
  } yield a + b

  println(both.run(20).value)

  import State._

  val prog: State[Int, (Int, Int, Int)] = for {
    a <- get[Int]
    _ <- set[Int](a + 1)
    b <- get[Int]
    _ <- modify[Int](_ + 1)
    c <- inspect[Int, Int](_ * 1000)
  } yield (a, b, c)

  type CalcState[A] = State[List[Int], A]

  def operand(n: Int): CalcState[Int] = State[List[Int], Int](s => (n :: s, n))

  def operator(f: (Int, Int) => Int): CalcState[Int] =
    State[List[Int], Int] {
      case x1 :: x2 :: rest =>
        val a = f(x1, x2)
        (a :: rest, a)
      case _ => sys.error("fail")
    }
  def evalOne(sym: String): CalcState[Int] = sym match {
      case "+" => operator(_ + _)
      case "-" => operator(_ - _)
      case "*" => operator(_ * _)
      case "/" => operator(_ / _)
      case _ => operand(sym.toInt)
  }
  // println(evalOne("42").run(Nil).value)
  val program = for {
    _ <- evalOne("1")
    _ <- evalOne("2")
    a <- evalOne("+")
  } yield a

  println(program.run(Nil).value)

  def evalAll(inp: List[String]): CalcState[Int] =
    inp.foldLeft(0.pure[CalcState]){ (c, i) => c.flatMap(_ => evalOne(i)) }

  val multistageProg = evalAll(List("1","2","+", "3", "*"))
  println(multistageProg.run(Nil).value)

  val biggerProgram = for {
    _ <- evalAll(List("1", "2", "+"))
    _ <- evalAll(List("4", "5", "+"))
    a <- evalOne("*")
  } yield a

  println(biggerProgram.run(Nil).value)

  def evalInput(inp: String): Unit = {
    evalAll(inp.split(" ").toList).runA(Nil).value
  }



}