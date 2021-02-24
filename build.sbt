import sbt.Keys.mainClass

val ScalatestVersion = "3.2.2"
val Http4sVersion = "0.21.16"
val CirceVersion = "0.13.0"
val MunitVersion = "0.7.20"
val LogbackVersion = "1.2.3"
val MunitCatsEffectVersion = "0.13.0"

val MainClass = "com.nicoburniske.blackjack_trainer.main.Main"

lazy val root = (project in file("."))
  .settings(
    organization := "com.nicoburniske",
    name := "blackjack-trainer",
    version := "0.0.1-SNAPSHOT",
    scalaVersion := "2.13.4",
    mainClass in(Compile, run) := Some(MainClass),
    libraryDependencies ++= Seq(
      "org.scalatic" %% "scalactic" % ScalatestVersion,
      "org.scalatest" %% "scalatest" % ScalatestVersion % "test",
      "org.http4s" %% "http4s-blaze-server" % Http4sVersion,
      "org.http4s" %% "http4s-blaze-client" % Http4sVersion,
      "org.http4s" %% "http4s-circe" % Http4sVersion,
      "org.http4s" %% "http4s-dsl" % Http4sVersion,
      "io.circe" %% "circe-generic" % CirceVersion,
      "org.scalameta" %% "munit" % MunitVersion % Test,
      "org.typelevel" %% "munit-cats-effect-2" % MunitCatsEffectVersion % Test,
      "ch.qos.logback" % "logback-classic" % LogbackVersion,
      "org.scalameta" %% "svm-subs" % "20.2.0"
    ),
    addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.10.3"),
    addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1"),
    testFrameworks += new TestFramework("munit.Framework")
  ).settings(
  test in assembly := {},
  mainClass in assembly := Some(MainClass),
  assemblyJarName in assembly := "blackjack-trainer.jar"
)

assemblyMergeStrategy in assembly := {
  case PathList("META-INF", _*) => MergeStrategy.discard
  case "module-info.class" => MergeStrategy.discard
  case x if x.endsWith("/module-info.class") => MergeStrategy.discard
  case _ => MergeStrategy.first
}