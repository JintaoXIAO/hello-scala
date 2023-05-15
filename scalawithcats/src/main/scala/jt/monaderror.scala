package monaderror

import cats.{ApplicativeError, MonadError}

object Main {

  def getCityClosestToCoordinate[F[_]](x: (Int, Int))(implicit ae: ApplicativeError[F, String]): F[String] =
    ae.pure("Minneapolis, MN")

  def getTemperatureByCity[F[_]](city: String)(implicit ae: ApplicativeError[F, String]): F[Int] =
    ae.pure(78)



}
