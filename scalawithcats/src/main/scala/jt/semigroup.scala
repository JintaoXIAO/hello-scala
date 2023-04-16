package semigroup


object Main extends App {
  import cats.Semigroupal
  import cats.instances.option._

  println(Semigroupal[Option].product(Some(12), Some("abc")))
  println(Semigroupal[Option].product(None, Some(12)))

  println(Semigroupal.tuple3[Option, Int, String, Boolean](Option(1), Option("hello"), Option(false)))

  import cats.syntax.apply._
  println((Option(123), Option("hello")).tupled)

  final case class Cat(name: String, born: Int, color: String)

  val c: Option[Cat] = (Option("Garfield"), Option(1978), Option("Orange & black")).mapN(Cat.apply)
  println(c)

  import cats.Monoid
  import cats.instances.int._
  import cats.instances.invariant._
  import cats.instances.list._
  import cats.instances.string._
  import cats.syntax.apply._

  final case class Dog(name: String, yearOfBirth: Int, favoriteFoods: List[String])

  val tupleToDog: (String, Int, List[String]) => Dog = Dog.apply
  val dogToTuple: Dog => (String, Int, List[String]) =
    dog => (dog.name, dog.yearOfBirth, dog.favoriteFoods)

  implicit val dogMonoid: Monoid[Dog] =
    (Monoid[String], Monoid[Int], Monoid[List[String]]).imapN(tupleToDog)(dogToTuple)

  import cats.instances.future._
  import scala.concurrent._
  import scala.concurrent.duration._
  import scala.concurrent.ExecutionContext.Implicits.global

  val futurePair = Semigroupal[Future].product(Future("Hello"), Future(123))

  val r: (String, Int) = Await.result(futurePair, 1.second)
  println(r)

  import cats.syntax.apply._

  val futureDog = (Future("Garfield"), Future(1989), Future(List("Lasagne"))).mapN(Dog.apply)

  val d = Await.result(futureDog, 1.second)
  println(d)

  import cats.instances.either._

  type ErrorOr[A] = Either[Vector[String], A]
  println(Semigroupal[ErrorOr].product(Left(Vector("Error 1")), Left(Vector("Error 2"))))

  import cats.syntax.parallel._
  val e1: ErrorOr[Int] = Left(Vector("Error 1"))
  val e2: ErrorOr[Int] = Left(Vector("Error 2"))
  println((e1, e2).parTupled)

  type ErrorOrList[A] = Either[List[String], A]
  var el1: ErrorOrList[Int] = Left(List("Error1", "Error2"))
  val el2: ErrorOrList[Int] = Left(List("Error3", "Error4"))
  println((el1, el2).parTupled)

  import cats.arrow.FunctionK
  object optionToList extends FunctionK[Option, List] {
    override def apply[A](fa: Option[A]): List[A] = fa match {
      case None => List.empty[A]
      case Some(a) => List(a)
    }
  }


}
