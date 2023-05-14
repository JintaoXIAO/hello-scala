package jt

object OrderMain extends App {
  import cats._
  import cats.syntax.all._

  val v1 = 1 > 2.0

  // val v2 = 1 compare 2.0 

}

object PartialOrderMain extends App {
  import cats._
  import cats.syntax.all._

  val v1 = 1 tryCompare 2  
  println(v1)

//  val v2 = 1 tryCompare "halo"  
  
}