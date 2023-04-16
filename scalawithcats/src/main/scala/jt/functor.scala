package functor

import cats.{Functor, Invariant, Monoid}
import cats.instances.list._
import cats.instances.option._

object Main extends App {

  val list1: List[Int] = List(1,2,3)
  val list2: List[Int] = Functor[List].map(list1)(_ * 2)

  val op1 = Option(123)
  val op2: Option[String] = Functor[Option].map(op1)(_.toString)

  val func: Int => Int = (x: Int) => x + 1
  val liftedFunc: Option[Int] => Option[Int] = Functor[Option].lift(func)
  import cats.syntax.functor._
  def doMath[F[_]](start: F[Int])(implicit functor: Functor[F]): F[Int] =
    start.map(x => x + 1 * 2)

  println(doMath(Option(20)))
  println(doMath(List(12,21)))

  case class Box[A](value: A)
  implicit val boxFunctor: Functor[Box] = new Functor[Box] {
    override def map[A, B](fa: Box[A])(f: A => B): Box[B] = Box(f(fa.value))
  }

  val box = Box[Int](123)
  println(box.map(a => a + 1))

  sealed trait Tree[+A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  case class Leaf[A](value: A) extends Tree[A]

  implicit val treeFunctor: Functor[Tree] = new Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
      case Leaf(a) => Leaf(f(a))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }
  }

  val tree: Tree[String] = Branch(Branch(Leaf("hello"), Branch(Leaf("world"), Leaf("from"))), Leaf("jintao"))
  println(tree.map(s => s.toUpperCase))

  trait Printable[A] { self =>
    def format(value: A): String
    def contramap[B](func: B => A): Printable[B] = new Printable[B] {
      override def format(value: B): String = self.format(func(value))
    }
  }

  def format[A](value: A)(implicit p: Printable[A]): String = p.format(value)

  implicit val strPrintable: Printable[String] = new Printable[String] {
    override def format(value: String): String = s"'$value'"
  }
  implicit val boolPrintable: Printable[Boolean] = new Printable[Boolean] {
    override def format(value: Boolean): String = if (value) "yes" else "no"
  }

  println(format("hello"))
  println(format(false))

  implicit def boxPrintable[A](implicit p: Printable[A]): Printable[Box[A]] = new Printable[Box[A]] {
    override def format(box: Box[A]): String = p.format(box.value)
  }

  println(format(Box("hello world")))
  println(format(Box(true)))

  trait Codec[A] { self =>
    def encode(value: A): String
    def decode(value: String): A
    def imap[B](dec: A => B, enc: B => A): Codec[B] = new Codec[B] {
      override def encode(value: B): String = self.encode(enc(value))
      override def decode(value: String): B = dec(self.decode(value))
    }
  }

  def encode[A](value: A)(implicit c: Codec[A]): String = c.encode(value)
  def decode[A](value: String)(implicit c: Codec[A]): A = c.decode(value)

  implicit val strCodec: Codec[String] = new Codec[String] {
    override def encode(value: String): String = value
    override def decode(value: String): String = value
  }

  implicit val intCodec: Codec[Int] = strCodec.imap(_.toInt, _.toString)
  implicit val boolCodec: Codec[Boolean] = strCodec.imap(_.toBoolean, _.toString)

  implicit def boxCodec[A](implicit c: Codec[A]): Codec[Box[A]] = new Codec[Box[A]] {
    override def encode(box: Box[A]): String = c.encode(box.value)
    override def decode(value: String): Box[A] = Box(c.decode(value))
  }

  println(encode(123))
  println(encode(Box(123)))
  println(decode[Boolean]("true"))
  println(decode[Box[Boolean]]("false"))


  import cats.Contravariant
  import cats.Show
  import cats.instances.string._

  val showString = Show[String]
  val showSymbol = Contravariant[Show].contramap(showString){ (sym: Symbol) => s"'${sym.name}'"}

  println(showSymbol.show(Symbol("dave")))

  import cats.syntax.contravariant._
  println(showString
    .contramap[Symbol](sym => s"'${sym.name}'")
    .show(Symbol("dave")))

  import cats.Monoid
  import cats.Invariant.catsInvariantMonoid
  import cats.syntax.invariant._
  import cats.syntax.semigroup._
  implicit val symbolMonoid: Monoid[Symbol] = Monoid[String].imap(Symbol.apply)(_.name)

  println(Symbol("a") |+| Symbol("b"))

}

