package study

object FoldableMain extends App{
  import cats._
  import cats.data._
  import cats.syntax.all._

  val v1 = Foldable[List].fold(List("hello", " world", " from", " jt")); println(v1)
  val v2 =Foldable[Option].fold(Some("hello world")); println(v2)
  val v3 = Foldable[Option].fold(None: Option[String]); println(v3)

  val a1 = Foldable[List].foldMap(List("hello", "world"))(_.length); println(a1)
  type MyError[A] = Either[String, A]
  val a2 = Foldable[MyError].foldMap("hello".asLeft[Int])(_.toString); println(a2)
}
