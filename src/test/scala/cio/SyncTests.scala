package cio

import cats.Eq
import cats.instances.AllInstances
import cats.syntax.AllSyntax
import cats.syntax.EqOps
import cats.tests.StrictCatsEquality
import org.scalatest.Assertion
import org.scalatest.freespec.AsyncFreeSpec

import scala.concurrent.Future

trait Tests extends StrictCatsEquality with AllInstances with AllSyntax {

  // disable Eq syntax (by making `catsSyntaxEq` not implicit), since it collides with scalactic's equality.
  override def catsSyntaxEq[A: Eq](a: A): EqOps[A] =
    new EqOps(a)
}

trait AsyncTests extends AsyncFreeSpec with Tests {

  implicit class CioAssertionOps[A <: Assertion](val c: CIO[A]) {
    def runTest: Future[Assertion] =
      CIO.runIO(c).unsafeToFuture
  }
}
