name := "HW"

version := "0.0.1"

scalaVersion := "2.11.7"

libraryDependencies += "com.jsuereth" %% "scala-arm" % "1.4"

// Breeze
libraryDependencies  ++= Seq(
  "org.scalanlp" %% "breeze" % "0.10",
  "org.scalanlp" %% "breeze-viz" % "0.11.2"
  //"org.scalanlp" %% "breeze-natives" % "0.10"
)

resolvers ++= Seq(
  // other resolvers here
  "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
)