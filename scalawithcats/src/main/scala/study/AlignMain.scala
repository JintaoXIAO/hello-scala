package study

object AlignMain extends App{
  import cats._
  import cats.data._
  import cats.syntax.all._

  val ior1 = "abc".leftIor[Int]; println(ior1)
  val ior2 = 123.rightIor[String]; println(ior2)
  val ior3 = Ior.Both("abc", 123); println(ior3)

  val v1 = ior1.leftMap(_.toUpperCase); println(v1)
  val v2 = ior2.map(_.toString + "!!!"); println(v2)
  val v3: String = ior3.fold(_.toUpperCase, _.toString, (a,b) => a + b.toString); println(v3)

  val ior4 = ior1.putLeft("hello"); println(ior4)
  val ior5 = ior1.putRight(12); println(ior5)
  val v4 = Align[List].align(List(1,2), List(10,11,12)); println(v4)

  val v5 = Align[Option].align(Some(10), Some("hello")); println(v5)
  val v6 = Align[Option].align(Some(10), None); println(v6)
  println(Align[Option].alignWith(Some(10), Some(11)){
    case Ior.Left(a) => a + 10
    case Ior.Right(b) => b * 3
    case Ior.Both(a, b) => a + b
  })

  println(
    Align[List].alignMergeWith(List(1,2,3), List(4,5,6,7)){ _ * _ }
  )
}