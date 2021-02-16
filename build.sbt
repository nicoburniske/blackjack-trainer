name := "blackjack-trainer"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.2"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.2" % "test"

lazy val commonSettings = Seq(
  version := "0.1-SNAPSHOT",
  organization := "org.nicoburniske",
  scalaVersion := "2.13.4",
  test in assembly := {}
)
mainClass in (Compile, run) := Some("com.nicoburniske.main.Main")

lazy val app = (project in file(".")).
  settings(commonSettings:_*).
  settings(
    mainClass in assembly := Some("com.nicoburniske.main.Main")
  )

assemblyMergeStrategy in assembly := {
  case PathList("META-INF", _*) => MergeStrategy.discard
  case "module-info.class" => MergeStrategy.discard
  case x if x.endsWith("/module-info.class") => MergeStrategy.discard
  case _ => MergeStrategy.first
}