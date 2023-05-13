import cats.{ Monoid, MonoidK }

Monoid[List[String]].empty

MonoidK[List].empty[String]

MonoidK[List].empty[Int]

MonoidK[List].combineK[String](List("hello", "world"), List("from", "scala"))