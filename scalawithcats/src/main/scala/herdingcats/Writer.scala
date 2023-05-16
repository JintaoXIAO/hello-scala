package herdingcats

object WriterMain extends App {
  import cats._
  import cats.syntax.all._

  def isBigGang(x: Int): (Boolean, String) = (x > 9, "Compared gang size to 9.")

  implicit class PairOps[A, B: Semigroup](pair: (A, B)) {
    def applyLog[C](f: A => (C, B)): (C, B) = {
      val (x, log) = pair
      val (x1, log1) = f(x)
      (x1, log |+| log1)
    }
  }

  val a = (3, "Smallish gang.") applyLog isBigGang
  println(a)

}
