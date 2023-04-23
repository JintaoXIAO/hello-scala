package rockjvm.part3

import cats.effect.{IO, IOApp}

import scala.concurrent.duration.DurationInt

object CancellingIOs extends IOApp.Simple {
  import rockjvm.utils._

  val chainOfIOs: IO[Int] = IO("waiting").myDebug >> IO.canceled >> IO(42)

  val specialPaymentSystem = {
    (
    IO("Payment running, don't cancel me...").myDebug >> IO.sleep(1.second) >> IO("Payment completed.").myDebug
      ).onCancel(IO("MEGA CANCEL OF DOOM!").myDebug.void)
  }

  val cancellationOfDoom = for {
    fib <- specialPaymentSystem.start
    _ <- IO.sleep(500.millis) >> fib.cancel
    _ <- fib.join
  } yield ()

  val atomicPayment = IO.uncancelable(_ => specialPaymentSystem)
  val atomicPayment_v2 = specialPaymentSystem.uncancelable

  val noCancellationOfDoom = for {
    fib <- atomicPayment.start
    _ <- IO.sleep(500.millis) >> IO("attempting cancellation...").myDebug >> fib.cancel
    _ <- fib.join
  } yield ()

  val inputPassword = IO("Input password: ").myDebug >> IO("typing password").myDebug >> IO.sleep(2.second) >> IO("RockJVM")
  val verifyPassword = (pw: String) => IO("verifying ...").myDebug >> IO.sleep(2.second) >> IO(pw == "RockJVM")

  val autoFlow: IO[Unit] = IO.uncancelable { poll =>
    for {
      pw <- poll(inputPassword).onCancel(IO("Authentication timeout out. Try again later").myDebug.void)
      verified <- verifyPassword(pw)
      _ <- if (verified) IO("Authentication Successful.").myDebug else IO("Authentication Failed.").myDebug
    } yield ()
  }

  val authProgram = for {
    fib <- autoFlow.start
    _ <- IO.sleep(1.seconds) >> IO("Authentication timeout, attempting cancel...").myDebug >> fib.cancel
  } yield ()

  val invincibleAuthProgram: IO[Unit] = for {
    authFib <- IO.uncancelable(_ => autoFlow).start
    _ <- IO.sleep(3.seconds) >> IO("Authentication timeout, attempting cancel...").myDebug >> authFib.cancel
    _ <- authFib.join
  } yield ()

  override def run: IO[Unit] = authProgram
}
