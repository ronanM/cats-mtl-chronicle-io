import cats.data.Ior
import cats.data.IorT
import cats.Applicative
import cats.Traverse
import cats.effect
import cats.effect.Concurrent
import cats.effect.ContextShift
import cats.effect.ExitCase
import cats.effect.IO
import cats.mtl.MonadChronicle
import cats.mtl.implicits._
import cats.syntax.all._
import cio.CIO.attemptUntitled
import cio.CIO.defaultOr
import cio.CIO.indent
import cio.LOG.LogItem.LogErr
import cio.LOG.LogItem.LogMsg
import cio.LOG.LogNode
import cio.LOG.printLog
import cio.LOG.withLastErr

import scala.Console.BOLD
import scala.Console.RED
import scala.Console.RESET
import scala.Console.WHITE
import scala.concurrent.duration.Duration.fromNanos

package object cio {

  object LOG {

    sealed trait LogItem {

      def dot: String =
        this match {
          case LogMsg(_, true) => "✅"
          case _               => "\uD83D\uDCA5" // 💥🔥
        }
    }

    object LogItem {
      final case class LogMsg(s: String, ok: Boolean = true) extends LogItem
      final case class LogErr(ex: Throwable) extends LogItem
    }

    final case class LogNode(value: LogItem, children: List[LogNode] = Nil) {

      def lastErr: Option[LogErr] =
        ((value match {
          case v: LogErr => List(v)
          case _         => Nil
        }) ++ children.mapFilter(_.lastErr)).lastOption

      def logStr(prefix: String = ""): String =
        ((value match {
          case v @ LogMsg(s, _) => s"  $prefix${v.dot} $BOLD$WHITE$s$RESET"
          case v @ LogErr(ex)   => s"  $prefix${v.dot} $BOLD$RED${ex.getMessage}$RESET"
        }) +: children.map(_.logStr(prefix + "  "))).mkString("\n")
    }

    def lastErr(log: LOG): Option[LogErr] =
      log.mapFilter(_.lastErr).lastOption

    def withLastErr[A](log: LOG)(f: LogErr => A): A =
      lastErr(log) match {
        case Some(logErr) => f(logErr)
        case None         => ???
      }

    def printLog(log: LOG): IO[Unit] =
      IO(println(s"CIO Log:\n\n${log.map(_.logStr()).mkString("\n")}\n"))
  }

  type LOG    = List[LogNode]
  type CIO[A] = IorT[IO, LOG, A]

  private val C = MonadChronicle[CIO, LOG]
  import C.chronicle
  import C.confess
  import C.dictate
  import C.materialize

  object CIO {
    def apply[A](body: => A): CIO[A] =
      fromIO(IO(body))

    def pure[A](a: A): CIO[A] =
      fromIO(IO.pure(a))

    def unit: CIO[Unit] =
      pure(())

    def fromIO[A](io: IO[A]): CIO[A] =
      IorT.liftF(io.attempt).flatMap {
        case Left(ex) => confess[A](List(LogNode(LogErr(ex))))
        case Right(a) => IorT.pure(a)
      }

    def defaultOr[A](c: CIO[A])(f: PartialFunction[Ior[LOG, A], CIO[A]]): CIO[A] =
      materialize(c).flatMap(f.orElse { case v => chronicle(v) })

    def transform[A, AA >: A](cio: CIO[A])(f: Ior[LOG, A] => Ior[LOG, AA]): CIO[AA] =
      materialize(cio).flatMap(v => chronicle(f(v)))

    def runIO[A](c: CIO[A]): IO[A] =
      c.value.flatMap {
        case Ior.Left(log)    => printLog(log) *> withLastErr(log) { case LogErr(ex) => IO.raiseError[A](ex) }
        case Ior.Right(a)     => IO.pure(a)
        case Ior.Both(log, a) => printLog(log).as(a)
      }

    def msg(s: String): CIO[Unit] =
      indent(s)(unit)

    def valued[A](c: CIO[A]): CIO[A] =
      defaultOr(c) {
        case Ior.Right(a)     => msg(s"Returned: $a").as(a)
        case Ior.Both(log, a) => indent(s"Returned: $a")(dictate(log)).as(a)
      }

    def timed[A](title: String)(c: CIO[A]): CIO[A] =
      CIO(fromNanos(System.nanoTime())).flatMap { start =>
        materialize(c).fproduct(_ => fromNanos(System.nanoTime())).flatMap { case (v, end) =>
          val timedTitle = s"$title (in ${(end - start).toMillis} ms)"
          v match {
            case Ior.Left(log)    => indent(timedTitle)(confess(log))
            case Ior.Right(a)     => pure(a)
            case Ior.Both(log, a) => indent(timedTitle)(dictate(log).as(a))
          }
        }
      }

    def indent[A](title: String, valued: Boolean = false)(c: CIO[A]): CIO[A] = {
      def titled(a: A) = if (valued) s"$title, Returned: $a" else title

      defaultOr(c) {
        case Ior.Left(log)    => confess(List(LogNode(LogMsg(title, ok = false), log)))
        case Ior.Right(a)     => dictate(List(LogNode(LogMsg(titled(a))))).as(a)
        case Ior.Both(log, a) => dictate(List(LogNode(LogMsg(titled(a)), log))).as(a)
      }
    }

    def traverse[F[_]: Traverse, A, B](title: String)(as: F[A])(f: A => CIO[B]): CIO[F[B]] =
      indent(s"$title (${as.size} values)")(as.traverse(f))

    def traverse_[F[_]: Traverse, A, B](title: String)(as: F[A])(f: A => CIO[B]): CIO[Unit] =
      indent(s"$title (${as.size} values)")(as.traverse_(f))

    def attemptUntitled[A](c: CIO[A]): CIO[Either[Throwable, A]] =
      materialize(c).flatMap {
        case Ior.Left(log)    => dictate(log).as(withLastErr(log)(_.ex.asLeft))
        case Ior.Right(a)     => pure(a.asRight)
        case Ior.Both(log, a) => dictate(log).as(a.asRight)
      }

    def whenA[A](cond: Boolean)(f: CIO[A]): CIO[Unit] =
      Applicative[CIO].whenA(cond)(f)
  }

  implicit def cioConcurrent(implicit cs: ContextShift[IO]): Concurrent[CIO] =
    new Concurrent[CIO] {

      val defaultConcurrent: Concurrent[CIO] = effect.Concurrent.catsIorTConcurrent(implicitly, implicitly)

      // Needed to drive static checks, otherwise the compiler will choke on type inference :-(
      type IorTLog[A] = IorT[IO, LOG, A]
      type Fiber[A]   = effect.Fiber[IorTLog, A]

      private def fibR[A](fib: effect.Fiber[CIO, Ior[LOG, A]]): Fiber[A] =
        effect.Fiber[IorTLog, A](fib.join.flatMap(chronicle), fib.cancel)

      override def start[A](fa: CIO[A]): CIO[Fiber[A]] =
        defaultConcurrent.start(fa)

      override def racePair[A, B](fa: CIO[A], fb: CIO[B]): CIO[Either[(A, Fiber[B]), (Fiber[A], B)]] =
        defaultConcurrent
          .racePair[Ior[LOG, A], Ior[LOG, B]](materialize(fa), materialize(fb))
          .flatMap {
            case Left((aa, fibB)) =>
              aa match {
                case Ior.Left(log)    => confess(log)
                case Ior.Right(a)     => pure(Left((a, fibR(fibB))))
                case Ior.Both(log, a) => dictate(log).as(Left((a, fibR(fibB))))
              }

            case Right((fibA, bb)) =>
              bb match {
                case Ior.Left(log)    => confess(log)
                case Ior.Right(b)     => pure(Right((fibR(fibA), b)))
                case Ior.Both(log, b) => dictate(log).as(Right((fibR(fibA), b)))
              }
          }

      override def async[A](k: (Either[Throwable, A] => Unit) => Unit): CIO[A] =
        defaultConcurrent.async(k)

      override def asyncF[A](k: (Either[Throwable, A] => Unit) => CIO[Unit]): CIO[A] =
        defaultConcurrent.asyncF(eitherF =>
          defaultOr(k(eitherF)) {
            case Ior.Left(log)    => indent("Async:")(confess(log))
            case Ior.Right(a)     => indent("Async:")(pure(a))
            case Ior.Both(log, a) => indent("Async:")(dictate(log).as(a))
          }
        )

      override def suspend[A](thunk: => CIO[A]): CIO[A] =
        defaultOr(defaultConcurrent.suspend(thunk)) {
          case Ior.Left(log)    => indent("Suspend:")(confess(log))
          case Ior.Right(a)     => indent("Suspend:")(pure(a))
          case Ior.Both(log, a) => indent("Suspend:")(dictate(log).as(a))
        }

      override def bracketCase[A, B](acquire: CIO[A])(use: A => CIO[B])(release: (A, ExitCase[Throwable]) => CIO[Unit]): CIO[B] =
        indent("Bracket:")(
          defaultConcurrent.bracketCase(defaultOr(acquire) {
            case Ior.Left(log)    => indent("Acquire:")(confess(log))
            case Ior.Both(log, a) => indent("Acquire:")(dictate(log).as(a))

          })(a =>
            defaultOr(use(a)) {
              case Ior.Left(log)    => indent("Use:")(confess(log))
              case Ior.Both(log, a) => indent("Use:")(dictate(log).as(a))
            }
          ) { case (aa, exitCase) =>
            defaultOr(release(aa, exitCase)) {
              case Ior.Left(log)    => indent(s"Release (exitCase: $exitCase)")(confess(log))
              case Ior.Both(log, a) => indent(s"Release (exitCase: $exitCase)")(dictate(log).as(a))
            }
          }
        )

      override def raiseError[A](e: Throwable): CIO[A] =
        CIO.fromIO(IO.raiseError(e))

      override def handleErrorWith[A](fa: CIO[A])(f: Throwable => CIO[A]): CIO[A] =
        defaultOr(fa) {
          case Ior.Left(log1) =>
            withLastErr(log1) { case LogErr(ex) =>
              materialize(f(ex)).flatMap {
                case Ior.Left(log2)    => indent(s"Not recovered:")(attemptUntitled(indent("First failed:")(confess(log1))) *> indent("Second failed:")(confess(log2)))
                case Ior.Right(a)      => indent(s"Recovered:")(indent("From:")(dictate(log1)).as(a))
                case Ior.Both(log2, a) => indent(s"Recovered:")(attemptUntitled(indent("From:")(confess(log1))) *> indent("To:")(dictate(log2).as(a)))
              }
            }
          case Ior.Right(a) => dictate(List(LogNode(LogMsg("Right (unuseful ?):")))).as(a)
        }

      override def flatMap[A, B](fa: CIO[A])(f: A => CIO[B]): CIO[B] =
        fa.flatMap(f)

      override def tailRecM[A, B](a: A)(f: A => CIO[Either[A, B]]): CIO[B] =
        defaultConcurrent.tailRecM(a)(f)

      override def pure[A](x: A): CIO[A] = CIO.pure(x)

      override def attempt[A](fa: CIO[A]): CIO[Either[Throwable, A]] =
        materialize(fa).flatMap {
          case Ior.Left(log)    => indent("Attempted:")(dictate(log).as(withLastErr(log)(_.ex.asLeft)))
          case Ior.Right(a)     => pure(a.asRight)
          case Ior.Both(log, a) => dictate(log).as(a.asRight)
        }

      override def ensureOr[A](fa: CIO[A])(error: A => Throwable)(predicate: A => Boolean): CIO[A] = {
        def check(a: A, log: LOG = Nil): CIO[A] =
          if (predicate(a)) pure(a)
          else indent("Ensure() fail")(confess(List(LogNode(LogErr(error(a))), LogNode(LogMsg("After:"), log))))

        defaultOr(fa) {
          case Ior.Right(a)     => check(a)
          case Ior.Both(log, a) => check(a, log)
        }
      }

      override def ensure[A](fa: CIO[A])(error: => Throwable)(predicate: A => Boolean): CIO[A] =
        ensureOr(fa)(_ => error)(predicate)
    }

  object syntax {

    implicit class TraverseCioOps[F[_]: Traverse, A](as: F[A]) {

      def traverseTitled[B](title: String, f: A => CIO[B]): CIO[F[B]] = CIO.traverse(title)(as)(f)

      def traverseTitled_[B](title: String, f: A => CIO[B]): CIO[Unit] = CIO.traverse_(title)(as)(f)
    }

    implicit class StringCioOps(val s: String) extends AnyVal {

      def ~<[A](c: CIO[A]): CIO[A] = CIO.indent(s)(c)

      def ~<<[A](c: CIO[A]): CIO[A] = CIO.indent(s, valued = true)(c)

      def ~~<[A](c: CIO[A]): CIO[A] = CIO.timed(s)(c)

      def ~*<[F[_]: Traverse, A, B](as: F[A])(f: A => CIO[B]): CIO[F[B]] =
        CIO.traverse(s)(as)(f)

      def `~<*_`[F[_]: Traverse, A, B](as: F[A])(f: A => CIO[B]): CIO[Unit] =
        CIO.traverse_(s)(as)(f)
    }

    implicit class CioOps[A](val c: CIO[A]) extends AnyVal {

      def valued: CIO[A] = CIO.valued(c)

      def titled(title: String): CIO[A] = CIO.indent(title)(c)

      def ~>(title: String): CIO[A] = CIO.indent(title)(c)

      def attemptUntitled: CIO[Either[Throwable, A]] = CIO.attemptUntitled(c)
    }
  }
}
