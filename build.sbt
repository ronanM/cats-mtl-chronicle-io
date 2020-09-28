scalaVersion := "2.12.12"

name := "cats-mtl-chronicle-io"

//target := file("/dev/shm/cio/target")

parallelExecution in ThisBuild := false

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.10")
resolvers += Resolver.sonatypeRepo("releases")

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "2.2.0",
  "org.typelevel" %% "cats-testkit-scalatest" % "2.0.0" % Test
)

libraryDependencies += "org.typelevel" %% "cats-effect" % "2.2.0"

resolvers += Resolver.bintrayRepo("edmundnoble", "maven")
libraryDependencies += "org.typelevel" %% "cats-mtl" % "1.0.0"

libraryDependencies += "co.fs2" %% "fs2-io" % "2.4.4"
