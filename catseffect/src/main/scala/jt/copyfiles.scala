package copyfiles

import cats.effect.{ExitCode, IOApp}

object Main extends IOApp {
  import cats.effect.{ IO, Resource }
  import java.io._
  import cats.effect.Sync
  import cats.syntax.all._

  def inputStream[F[_]: Sync](f: File): Resource[F, FileInputStream] =
    Resource.make(Sync[F].blocking(new FileInputStream(f))) { inStream =>
      Sync[F].blocking(inStream.close()).handleErrorWith(_ => Sync[F].unit)
    }

  def outputStream[F[_]: Sync](f: File): Resource[F, FileOutputStream] =
    Resource.fromAutoCloseable(Sync[F].blocking(new FileOutputStream(f)))

  def inputOutputStream[F[_]: Sync](in: File, out: File): Resource[F, (InputStream, OutputStream)] =
    for {
      inStream <- inputStream(in)
      outStream <- outputStream(out)
    } yield (inStream, outStream)

  def transmit[F[_]: Sync](origin: InputStream, destination: OutputStream, buffer: Array[Byte], acc: Long): F[Long] =
    for {
      amount <- Sync[F].blocking(origin.read(buffer, 0, buffer.size))
      count <-
        if(amount > -1)
          Sync[F].blocking(destination.write(buffer, 0, amount)) >> Sync[F].interruptible() >> transmit(origin, destination, buffer, acc + amount)
        else Sync[F].pure(acc)

    } yield count

  def transfer[F[_]: Sync](origin: InputStream, destination: OutputStream): F[Long] =
    transmit[F](origin, destination, new Array[Byte](1024 * 10), 0L)

  def copy(origin: File, destination: File): IO[Long] = {
    val inIO: IO[InputStream] = IO(new FileInputStream(origin))
    val outIO: IO[OutputStream] = IO(new FileOutputStream(destination))
    (inIO, outIO)
      .tupled
      .bracket{ case (in, out) =>
        transfer[IO](in, out)
      } { case (in, out) =>
        (IO(in.close()), IO(out.close())).tupled.void.handleErrorWith(_ => IO.unit)
      }
  }

  override def run(args: List[String]): IO[ExitCode] =
    for {
      _ <- if (args.length < 2) IO.raiseError(new IllegalArgumentException("Need origin and destination files")) else IO.unit
      orig = new File(args(0))
      dest = new File(args(1))
      _ <- if (orig == dest) IO.raiseError(new IllegalArgumentException("Origin and dest are same")) else IO.unit
      _ <- if (orig.canRead && dest.canWrite) IO.unit else IO.raiseError(new IllegalArgumentException("permission check failed"))
      _ <- if(dest.exists()) IO.raiseError(new IllegalArgumentException("Dest already exists")) else IO.unit

      count <- copy(orig, dest)
      _ <- IO.println(s"$count bytes copied from ${orig.getPath} to ${dest.getPath}")
    } yield ExitCode.Success

}
