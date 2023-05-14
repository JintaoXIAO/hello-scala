package jt

/*
trait Monad extends FlatMap[F] with Applicative[F] {

}
*/
package object demo {
  import cats._
  import cats.syntax.all._


  type Birds = Int

  object Demo01 {
    case class Pole(left: Birds, right: Birds) {
      def landLeft(n: Birds): Pole = copy(left = left + n) 
      def landRight(n: Birds): Pole = copy(right = right + n) 
    }

    val v = Pole(0, 0).landLeft(2).landLeft(-1).landRight(2)
  }

  object Demo02 {
    case class Pole(left: Birds, right: Birds) {
      def landLeft(n: Birds): Option[Pole] =
        if (math.abs((left + n) - right) < 4) copy(left = left + n).some
        else none[Pole]
      def landRight(n: Birds): Option[Pole] =
        if (math.abs(left - (right + n)) < 4) copy(right = right + n).some
        else none[Pole]
    }

    //val v = Pole(0, 0).landLeft(1).landRight(3)

  }


}

object MonadMain extends App {
  import cats._
  import cats.syntax.all._  
  import demo.Demo02._
  
  val v = Monad[Option].pure(Pole(0, 0)) >>= { _.landLeft(2) } >>= 
      { _.landLeft(1) } >>= { _.landRight(3) } 
  println(v)

}