// scalaVersion := "2.11.6"
scalaVersion := "2.12.9"

scalacOptions ++= Seq("-unchecked","-deprecation","-feature")

libraryDependencies += "nz.ac.waikato.cms.weka" % "weka-dev" % "3.7.12"

// libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4"

//  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.1",
//  "org.scala-lang.modules" %% "scala-swing" % "1.0.1",

libraryDependencies += "com.github.scopt" %% "scopt" % "3.7.0"

resolvers += Resolver.sonatypeRepo("public")

resolvers += Resolver.sonatypeRepo("public")

libraryDependencies += "com.github.scopt" %% "scopt" % "3.2.0"
