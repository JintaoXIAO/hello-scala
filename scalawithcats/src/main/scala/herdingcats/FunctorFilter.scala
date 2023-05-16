package herdingcats

object FunctorFilterMain extends App {
  import cats._
  import cats.syntax.all._

  val english: Map[Int, String] = Map(1 -> "one", 3 -> "three", 10 -> "ten")
  val v = (1 to 40).toList mapFilter { english.get }
  println(v)

  def collectingEnglish[F[_]: FunctorFilter](f: F[Int]): F[String] =
    f collect {
      case 1 => "one"
      case 3 => "three"
      case 10 => "ten"
    }
}
