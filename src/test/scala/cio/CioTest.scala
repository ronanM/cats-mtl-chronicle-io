package cio

import cats.effect.Bracket
import cats.effect.IO
import cats.effect.implicits._
import cio.CIO.fromIO
import cio.syntax._
import org.scalatest.Succeeded

import scala.concurrent.duration.DurationInt

class CioTest extends AsyncTests {

  var fi = 1

  val good: CIO[String]  = CIO.msg("Doing Good things").as("Good")
  val good2: CIO[String] = CIO.msg("Doing Good things2").as("Good2")
  val bad: CIO[String]   = CIO.fromIO(IO.raiseError(new Throwable("Error when doing Bad things")))
  val bad2: CIO[String]  = CIO.fromIO(IO.raiseError(new Throwable("Error when doing Bad things2")))

  def doThat(s: String): CIO[String] =
    if (s.toLowerCase.contains("bad"))
      fromIO(IO.raiseError(new Throwable(s"Error during: $s")))
    else CIO.msg(s).as(s)

  def f(i: Int = 0): CIO[Int] =
    if (i === 0) {
      fi += 1
      f(fi)
    } else if (i < 0) fromIO(IO.raiseError(new Throwable(s"Error with input: $i")))
    else CIO.msg(s"Do with input: $i").as(i)

  implicit val ioCS    = IO.contextShift(implicitly)
  implicit val ioTimer = IO.timer(implicitly)

  val bracketIO: Bracket[IO, Throwable]   = implicitly
  val bracketCIO: Bracket[CIO, Throwable] = cio.cioConcurrent(ioCS)

  "CIO" in {
    for {
      _ <- "Basics" ~< (for {
        _ <- good
        _ <- good.titled("With title")
        _ <- "Using ~< for indentation" ~< good
        _ <- "Using ~~< for timed indentation" ~~< (good *> CIO.fromIO(IO.sleep(200.millis))).replicateA(3)
        _ <- good ~> "Using ~> for indentation"
        _ <- "Using .valued for getting the value" ~< good.valued
        _ <- good.as((42, "A String value")).valued
      } yield ())

      _ <- "Error management" ~< (for {
        _ <- "Using .attempt to handle errors by turning them into Either values" ~< (
          good.attempt.valued *> bad.attempt.valued
        )
        _ <- "Using .attemptUntitled to handle errors by turning them into Either values" ~< (
          good.attemptUntitled.valued *> bad.attemptUntitled.valued
        )

        _ <- "Using handleErrorWith()" ~< (for {
          _ <- "all is Ok" ~< good.handleErrorWith(ex => good2).valued
          _ <- "1 NOk then 1 Ok" ~< bad.handleErrorWith(ex => good2).valued
          _ <- "1 NOk then 1 NOk" ~< bad.handleErrorWith(ex => bad2).attemptUntitled.valued
        } yield ())

        _ <- "Using redeem()" ~~< (for {
          _ <- ("redeem from Ok" ~< good.redeem(ex => s"Recovered from '${ex.getMessage}'", _.toUpperCase)).valued
          _ <- ("redeem from NOk" ~< bad.redeem(ex => s"Recovered from '${ex.getMessage}'", _.toUpperCase)).valued
        } yield ())

        _ <- f(50).ensureOr(i => new Throwable(s"$i is too small (<100)"))(_ > 100).attemptUntitled
        _ <- f(50).ensure(new Throwable("Too small (<100)"))(_ > 100).attemptUntitled
      } yield ())

      _ <- "Traverse" ~< (for {
        _ <- ("Classic traverse Ok" ~< List(7, 8, 9).traverse(f)).attemptUntitled.valued
        _ <- ("Classic traverse NOk" ~< List(7, -8, 9).traverse(f)).attemptUntitled.valued

        _ <- ("Titled traverse() Ok" ~*< 40.some)(f).valued
        _ <- ("Titled empty traverse() Ok" ~*< none[Int])(f).valued

        _ <- ("Titled traverse() Ok" ~*< List(1, 2, 3))(f).valued
        _ <- ("Titled traverse_() Ok (result ignored)" `~<*_` List(1, 2, 3))(f).valued

        _ <- ("Traverse NOk" `~<*_` List(1, -2, 3))(f).attemptUntitled
      } yield ()).attemptUntitled

      _ <- "Applicative (.tupled, .mapN, parTupled, etc.)" ~< {
        val goodTuple = (f(1), f(2), f(3))
        val badTuple  = (f(1), f(-2), f(3))

        for {
          _ <- (".tupled Ok" ~< goodTuple.tupled).attemptUntitled.valued
          _ <- (".tupled NOk" ~< badTuple.tupled).attemptUntitled.valued

          _ <- (".mapN Ok" ~< goodTuple.mapN(_ + _ + _)).attemptUntitled.valued
          _ <- (".mapN NOk" ~< badTuple.mapN(_ + _ + _)).attemptUntitled.valued

          _ <- (".parTupled Ok" ~< goodTuple.parTupled).attemptUntitled.valued
          _ <- (".parTupled NOk" ~< badTuple.parTupled).attemptUntitled.valued

          _ <- (".parMapN Ok" ~< goodTuple.parMapN(_ + _ + _)).attemptUntitled.valued
          _ <- (".parMapN NOk" ~< badTuple.parMapN(_ + _ + _)).attemptUntitled.valued
        } yield ()
      }

      _ <- "Bracket" ~< (for {
        _ <- ("Bracket Ok" ~< f().bracket[Int](i => f(i * 10))(i => good.void)).attemptUntitled.valued
        _ <- ("Bracket NOk (acquire Nok)" ~< f(-1).bracket[Int](i => f(i * 10))(i => good.void)).attemptUntitled.valued
        _ <- ("Bracket NOk (use Nok)" ~< f().bracket[Int](i => f(-i * 10))(i => good.void)).attemptUntitled.valued
        _ <- ("Bracket NOk (use Nok)" ~< f().bracket[Int](i => f(-i * 10))(i => good.void)).attemptUntitled.valued
        _ <- ("Bracket NOk (release Nok)" ~< f().bracket[Int](i => f(i * 10))(i => bad.void)).attemptUntitled.valued
      } yield ())

      _ <- CIO.whenA(false) {
        for {
          fsResult <- "Using FS2" ~<
            fs2
              .Stream(1, 2, 3)
              .covary[CIO]
              .mapAsync(1)(i => f(i * 1000).valued)
              .compile
              .toList

          _ <- CIO(println(s"fsResult: $fsResult"))
          _ <- CIO(assert(fsResult === List(1000, 2000, 3000)))
        } yield ()
      }
    } yield Succeeded
  }.runTest

  "Example" in {
    def badIfEven(i: Int) = if (i % 2 === 0) "(bad)" else ""

    ("Example program" ~<< {
      for {
        ints <- "Get integers" ~~< {
          doThat("Read integers from bad server")
            .as(List(1, 2, 3))
            .handleErrorWith(_ => doThat("Read integers from rescue server").as(List(7, 8, 9)).valued)
        }

        rs <- ("For each integers" ~*< ints) { i =>
          s"With $i" ~<<
            doThat(s"Produce result for the input $i ${badIfEven(i)}")
              .as(i * 33)
              .handleErrorWith(ex => doThat(s"Use more expensive service for $i").as(i * 1000).valued)
              .flatTap(v => doThat(s"Write '$v' in ${badIfEven(v)} log  file").attempt)
        }
      } yield rs
    }).map(v => assert(v === List(231, 8000, 297))).runTest
  }
}
