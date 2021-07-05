scalaVersion := "2.12.14"

name := "cats-mtl-chronicle-io"

//target := file("/dev/shm/cio/target")

ThisBuild / parallelExecution := false

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.10")
resolvers += Resolver.sonatypeRepo("releases")

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "2.6.1",
  "org.typelevel" %% "cats-testkit-scalatest" % "2.1.5" % Test
)

libraryDependencies += "org.typelevel" %% "cats-effect" % "2.5.1"

resolvers += Resolver.bintrayRepo("edmundnoble", "maven")
libraryDependencies += "org.typelevel" %% "cats-mtl" % "1.2.1"

libraryDependencies += "co.fs2" %% "fs2-io" % "2.5.8"
