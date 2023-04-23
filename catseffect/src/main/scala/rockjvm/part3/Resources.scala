package rockjvm.part3

import cats.effect.{IO, IOApp, Resource}

import java.io.{File, FileReader}
import java.util.Scanner
import scala.concurrent.duration.DurationInt
import cats.effect.kernel.Outcome
import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}

object Resources extends IOApp.Simple {

  import rockjvm.utils._
  class Connection(url: String) {
    def open: IO[String] = IO(s"opening connection to $url")
    def close: IO[String] = IO(s"closing connection to $url")
  }

  val asyncFetchUrl: IO[Unit] = for {
    fib <- (new Connection("rockjvm").open *> IO.sleep(Int.MaxValue.seconds)).start
    _ <- IO.sleep(1.second) *> fib.cancel
  } yield ()

  val correctAsyncFetchUrl: IO[Unit] = for {
    conn <- IO(new Connection("rockjvm"))
    fib <- (conn.open *> IO.sleep(Int.MaxValue.seconds)).onCancel(conn.close.void).start
    _ <- IO.sleep(1.second) *> fib.cancel
  } yield ()

  val bracketFetchUrl: IO[Unit] = IO(new Connection("rockjvm.com"))
    .bracket(conn => conn.open *> IO.sleep(Int.MaxValue.second))(_.close.void)

  val bracketProgram: IO[Unit] = for {
    fib <- bracketFetchUrl.start
    _ <- IO.sleep(1.second) *> fib.cancel
  } yield ()

  def openFileScanner(path: String): IO[Scanner] =
    IO(new Scanner(new FileReader(new File(path))))

  def readLineByLine(sc: Scanner): IO[Unit] =
    if (sc.hasNextLine) IO(sc.nextLine()).myDebug >> IO.sleep(100.millis) >> readLineByLine(sc)
    else IO.unit

  def bracketReadFile(path: String): IO[Unit] =
    IO(s"opening file at $path") >>
      openFileScanner(path).bracket(readLineByLine)(sc => IO(s"closing file at $path").myDebug >> IO(sc.close()))

  def connFromConfig(path: String): IO[Unit] =
    openFileScanner(path).bracket{ sc =>
      IO(new Connection(sc.nextLine())).bracket{ conn =>
        conn.open.myDebug >> IO.never
      }(conn => conn.close.void)
    }(sc => IO("closing file").myDebug>> IO(sc.close()))

  val connectionResource: Resource[IO, Connection] =
    Resource.make(IO(new Connection("rocketjvm")))(conn => conn.close.myDebug.void)

  val resourceFetchUrl = for {
    fib <- connectionResource.use(conn => conn.open.myDebug >> IO.never).start
    _ <- IO.sleep(1.second) >> fib.cancel
  } yield ()

  val simpleResource = IO("some resource")
  val usingResource = (string: String) => IO(s"using the string: $string").myDebug
  val releaseResource: String => IO[Unit] = string => IO(s"finaliziung the string: $string").myDebug.void

  val usingResourceWithBracket = simpleResource.bracket(usingResource)(releaseResource)
  val usingResourceWithResource = Resource.make(simpleResource)(releaseResource).use(usingResource)


  def getResourceFromFile(path: String) = Resource.make(openFileScanner(path)) { sc =>
    IO(s"closing file at $path").myDebug >> IO(sc.close())
  }

  def resourceReadFile(path: String) =
    IO(s"opening file at $path").myDebug >> getResourceFromFile(path).use { sc =>
      readLineByLine(sc)
    }

  def cancelReadFile(path: String) = for {
    fib <- resourceReadFile(path).start
    _ <- IO.sleep(2.second) >> fib.cancel
  } yield ()

  def connFromConfResource(path: String) =
    Resource.make(openFileScanner(path))(sc => IO("closing file").myDebug >> IO(sc.close()))
      .flatMap(sc => Resource.make(IO(new Connection(sc.nextLine())))(conn => conn.close.void))

  def openConnection = connFromConfResource("")

  override def run: IO[Unit] = cancelReadFile("C:\\Users\\xiaoj\\.spacemacs")
}
