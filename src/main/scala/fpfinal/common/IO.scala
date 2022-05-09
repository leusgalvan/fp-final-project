package fpfinal.common

import cats._

import scala.annotation.tailrec
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

trait IO[+A] {
  import IO._

  private def runWithEH[AA >: A](eh: Option[ErrorHandler[AA]]): AA = {
    resume(this, eh) match {
      case Right(a) => a
      case Left(res) =>
        val (nextIO, nextEh) = res()
        nextIO.runWithEH(nextEh)
    }
  }

  def run: A = runWithEH(None)
}

object IO {
  type ErrorHandler[A] = Throwable => IO[A]

  case class Done[A](a: A) extends IO[A]
  case class More[A](f: () => IO[A]) extends IO[A]
  case class FlatMap[A, B](ta: IO[A], f: A => IO[B]) extends IO[B]
  case class RaiseError(error: Throwable) extends IO[Nothing]
  case class HandleErrorWith[A](ioa: IO[A], f: Throwable => IO[A]) extends IO[Nothing]

  def safeApply[A](a: => IO[A]): IO[A] =
    Try(a) match {
      case Success(ioa)         => ioa
      case Failure(NonFatal(e)) => RaiseError(e)
      case Failure(e)           => throw e // rethrow fatal errors
    }

  @tailrec
  final def resume[A](io: IO[A], eh: Option[ErrorHandler[A]]): Either[() => (IO[A], Option[ErrorHandler[A]]), A] =
    io match {
      case Done(a)       => Right(a)
      case More(thunk)   => resume(safeApply(thunk()), eh)
      case RaiseError(e) =>
        eh match {
          case Some(eha) => resume(eha(e), None)
          case None      => throw e
        }
      case HandleErrorWith(ioa, newEh) => resume(ioa.asInstanceOf[IO[A]], Some(newEh.asInstanceOf[ErrorHandler[A]]))
      case FlatMap(t, f) =>
        t match {
          case Done(a2)        => resume(safeApply(f(a2)), eh)
          case More(thunk2)    => Left(() => (FlatMap(safeApply(thunk2()), f), eh))
          case RaiseError(e)   => resume(RaiseError(e), eh)
          case HandleErrorWith(ioa, eh2) => resume(FlatMap(ioa, f), Some(eh2.asInstanceOf[ErrorHandler[A]]))
          case FlatMap(t2, f2) =>
            resume(FlatMap(t2, (x: Any) => FlatMap(safeApply(f2(x)), f)), eh)
        }
    }

  def apply[A](a: => A): IO[A] = suspend(a)

  def raiseError(e: Throwable): IO[Nothing] =
    RaiseError(e)

  def suspend[A](a: => A): IO[A] =
    More { () => Done(a) }

  implicit val ioMonad: MonadError[IO, Throwable] = new MonadError[IO, Throwable] {
    override def pure[A](x: A): IO[A] = Done(x)

    override def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = FlatMap(fa, f)

    override def tailRecM[A, B](a: A)(f: A => IO[Either[A, B]]): IO[B] = {
      flatMap(f(a)) {
        case Left(a)  => tailRecM(a)(f)
        case Right(b) => pure(b)
      }
    }

    override def raiseError[A](e: Throwable): IO[A] = RaiseError(e)

    override def handleErrorWith[A](fa: IO[A])(f: Throwable => IO[A]): IO[A] = HandleErrorWith(fa, f)
  }
}
