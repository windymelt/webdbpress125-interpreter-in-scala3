import sbt._

object Dependencies {
  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.2.9"
  lazy val parboiled2 =
    ("org.parboiled" %% "parboiled" % "2.3.0").cross(CrossVersion.for3Use2_13)
}
