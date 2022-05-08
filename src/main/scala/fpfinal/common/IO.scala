package fpfinal.common

import cats._

import scala.annotation.tailrec

/**
 * Simple implementation of the IO monad which provides the ability to wrap pure values,
 * suspend computations and combine them via flatMap.
 *
 * Uses a trampoline to avoid stack overflows.
 */
trait IO[+A] {
  import IO._

  @tailrec
  final def run: A =
    resume(this) match {
      case Right(a)    => a
      case Left(thunk) => thunk().run
    }
}

object IO {
  case class Done[A](a: A) extends IO[A]
  case class More[A](f: () => IO[A]) extends IO[A]
  case class FlatMap[A, B](ta: IO[A], f: A => IO[B]) extends IO[B]

  @tailrec
  final def resume[A](io: IO[A]): Either[() => IO[A], A] =
    io match {
      case Done(a)     => Right(a)
      case More(thunk) => resume(thunk())
      case FlatMap(t, f) =>
        t match {
          case Done(a2)     => resume(f(a2))
          case More(thunk2) => Left(() => FlatMap(thunk2(), f))
          case FlatMap(t2, f2) =>
            resume(FlatMap(t2, (x: Any) => FlatMap(f2(x), f)))
        }
    }

  def apply[A](a: => A): IO[A] = suspend(a)

  def suspend[A](a: => A): IO[A] =
    More(() => Done(a))

  implicit val ioMonad: Monad[IO] = new Monad[IO] {
    override def pure[A](x: A): IO[A] = Done(x)

    override def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = FlatMap(fa, f)

    override def tailRecM[A, B](a: A)(f: A => IO[Either[A, B]]): IO[B] = {
      flatMap(f(a)) {
        case Left(a)  => tailRecM(a)(f)
        case Right(b) => pure(b)
      }
    }
  }
}
