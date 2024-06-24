// scalaVersion := "2.11.6"
ThisBuild / scalaVersion := "2.12.9"
ThisBuild / githubWorkflowJavaVersions := Seq(JavaSpec.temurin("11"))

scalacOptions ++= Seq("-unchecked","-deprecation","-feature")

libraryDependencies += "nz.ac.waikato.cms.weka" % "weka-dev" % "3.7.12"

// libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4"

//  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.1",
//  "org.scala-lang.modules" %% "scala-swing" % "1.0.1",

libraryDependencies += "com.github.scopt" %% "scopt" % "3.7.0"

resolvers ++= Resolver.sonatypeOssRepos("public")

lazy val root = (project in file("."))
  .aggregate(`scalatest-otel-reporter`)
  .settings(
    publish / skip := true,
  )

  lazy val `scalatest-otel-reporter` = (project in file("scalatest-otel-reporter"))
  .settings(
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.16" % Provided,
      "io.opentelemetry" % "opentelemetry-sdk" % "1.30.0" % Provided,
    ),
  )