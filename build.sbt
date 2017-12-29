/*
 * Copyright 2013 - 2017 Outworkers Ltd.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
import sbt.Keys._
import sbt._

lazy val Versions = new {
  val logback = "1.2.3"
  val util = "0.38.0"
  val json4s = "3.5.1"
  val datastax = "3.3.2"
  val scalatest = "3.0.4"
  val shapeless = "2.3.2"
  val scalacheck = "1.13.5"
  val slf4j = "1.7.25"
  val joda = "2.9.9"
  val jodaConvert = "1.8.1"
  val macrocompat = "1.1.1"
  val macroParadise = "2.1.0"
  val circe = "0.8.0"
  val playAnorm = "2.5.3"
  
  val scala = new {
    val 210 = "2.10.6"
    val 211 = "2.11.11"
    val 212 = "2.12.4"
    val all = Seq(scala210, scala211, scala212)
  }

}

lazy val ScalacOptions = Seq(
  "-deprecation", // Emit warning and location for usages of deprecated APIs.
  "-encoding",
  "utf-8", // Specify character encoding used by source files.
  "-feature",
  "-explaintypes", // Explain type errors in more detail.
  "-feature", // Emit warning and location for usages of features that should be imported explicitly.
  "-language:reflectiveCalls",
  "-language:postfixOps",
  "-language:existentials", // Existential types (besides wildcard types) can be written and inferred
  "-language:experimental.macros", // Allow macro definition (besides implementation and application)
  "-language:higherKinds", // Allow higher-kinded types
  "-language:implicitConversions", // Allow definition of implicit functions called views
  "-unchecked", // Enable additional warnings where generated code depends on assumptions.
  "-Xcheckinit", // Wrap field accessors to throw an exception on uninitialized access.
  //"-Xfatal-warnings", // Fail the compilation if there are any warnings.
  "-Xfuture" // Turn on future language features.
  //"-Yno-adapted-args" // Do not adapt an argument list (either by inserting () or creating a tuple) to match the receiver.
)

val XLintOptions = Seq(
  "-Xlint:adapted-args", // Warn if an argument list is modified to match the receiver.
  "-Xlint:by-name-right-associative", // By-name parameter of right associative operator.
  "-Xlint:constant", // Evaluation of a constant arithmetic expression results in an error.
  "-Xlint:delayedinit-select", // Selecting member of DelayedInit.
  "-Xlint:doc-detached", // A Scaladoc comment appears to be detached from its element.
  "-Xlint:inaccessible", // Warn about inaccessible types in method signatures.
  "-Xlint:missing-interpolator", // A string literal appears to be missing an interpolator id.
  "-Xlint:nullary-override", // Warn when non-nullary `def f()' overrides nullary `def f'.
  "-Xlint:nullary-unit", // Warn when nullary methods return Unit.
  "-Xlint:option-implicit", // Option.apply used implicit view.
  "-Xlint:package-object-classes", // Class or object defined in package object.
  "-Xlint:poly-implicit-overload", // Parameterized overloaded implicit methods are not visible as view bounds.
  "-Xlint:private-shadow", // A private field (or class parameter) shadows a superclass field.
  "-Xlint:stars-align", // Pattern sequence wildcard must align with sequence component.
  "-Xlint:type-parameter-shadow", // A local type parameter shadows a type already in scope.
  "-Xlint:unsound-match" // Pattern match may not be typesafe.
)

val Scala212Options = Seq(
  "-Xlint:infer-any", // Warn when a type argument is inferred to be `Any`.
  "-Ypartial-unification", // Enable partial unification in type constructor inference,
  "-Ywarn-extra-implicit", // Warn when more than one implicit parameter section is defined.
  "-Ywarn-unused:implicits", // Warn if an implicit parameter is unused.
  "-Ywarn-unused:imports", // Warn if an import selector is not referenced.
  "-Ywarn-unused:locals", // Warn if a local definition is unused.
  "-Ywarn-unused:params", // Warn if a value parameter is unused.
  "-Ywarn-unused:patvars", // Warn if a variable bound in a pattern is unused.
  "-Ywarn-unused:privates" // Warn if a private member is unused.
) ++ XLintOptions

val YWarnOptions = Seq(
  "-Ywarn-dead-code", // Warn when dead code is identified.
  "-Ywarn-inaccessible", // Warn about inaccessible types in method signatures.
  "-Ywarn-nullary-override", // Warn when non-nullary `def f()' overrides nullary `def f'.
  "-Ywarn-nullary-unit", // Warn when nullary methods return Unit.
  "-Ywarn-numeric-widen", // Warn when numerics are widened.
  "-Ywarn-value-discard" // Warn when non-Unit expression results are unused.
)

val scalacOptionsFn: String => Seq[String] = { s =>
  CrossVersion.partialVersion(s) match {
    case Some((_, minor)) if minor >= 12 => ScalacOptions ++ YWarnOptions ++ Scala212Options
    case _ => ScalacOptions ++ YWarnOptions
  }
}

scalacOptions in ThisBuild ++= ScalacOptions ++ YWarnOptions

val defaultConcurrency = 4

val sharedSettings: Seq[Def.Setting[_]] = Defaults.coreDefaultSettings ++ Seq(
  organization := "com.outworkers",
  scalaVersion := Versions.scala.212,
  credentials ++= Publishing.defaultCredentials,
  resolvers ++= Seq(
    Resolver.typesafeRepo("releases"),
    Resolver.sonatypeRepo("releases"),
    Resolver.jcenterRepo
  ),
  logLevel in ThisBuild := Level.Info,
  libraryDependencies ++= Seq(
    "ch.qos.logback" % "logback-classic" % Versions.logback % Test,
    "org.slf4j" % "log4j-over-slf4j" % Versions.slf4j
  ),
  fork in Test := true,

  scalacOptions ++= scalacOptionsFn(scalaVersion.value),
  scalacOptions in (Compile, console) := ScalacOptions.filterNot(
    Set(
      "-Ywarn-unused:imports",
      "-Xfatal-warnings"
    )
  ),
  javaOptions in Test ++= Seq(
    "-Xmx2G",
    "-Djava.net.preferIPv4Stack=true",
    "-Dio.netty.resourceLeakDetection"
  ),
  envVars := Map("SCALACTIC_FILL_FILE_PATHNAMES" -> "yes"),
  parallelExecution in ThisBuild := false
) ++ Publishing.effectiveSettings

lazy val root = (project in file("."))
  .settings(
    sharedSettings ++ Publishing.noPublishSettings
  ).settings(
    name := "sql-parser",
    moduleName := "sql-parser",
    pgpPassphrase := Publishing.pgpPass,
    commands += Command.command("testsWithCoverage") { state =>
      "coverage" ::
      "test" ::
      "coverageReport" ::
      "coverageAggregate" ::
      "coveralls" ::
      state
    }
  ).aggregate(
    sqlParser,
    readme
  )

lazy val sqlParser = (project in file("parser"))
  .settings(sharedSettings: _*)
  .settings(
    name := "sql-parser-dsl",
    moduleName := "sql-parser-dsl",
    crossScalaVersions := Versions.scala.all,
    concurrentRestrictions in Test := Seq(
      Tags.limit(Tags.ForkedTestGroup, defaultConcurrency)
    ),
    libraryDependencies ++= Seq(
      "com.typesafe.play" %% "anorm" % Versions.playAnorm,
      "com.github.mauricio" %% "postgresql-async" % Versions.postgresAsync,
      "org.typelevel" %% "macro-compat" % Versions.macrocompat,
      "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided",
      compilerPlugin("org.scalamacros" % "paradise" % Versions.macroParadise cross CrossVersion.full),
      "com.chuusai"                  %% "shapeless" % Versions.shapeless,
      "joda-time"                    %  "joda-time" % Versions.joda,
      "org.joda"                     %  "joda-convert" % Versions.jodaConvert
    )
  )


lazy val readme = (project in file("readme"))
  .settings(sharedSettings)
  .settings(
    publishArtifact := false,
    crossScalaVersions := Seq(Versions.scala.211, Versions.scala.212),
    tutSourceDirectory := sourceDirectory.value / "main" / "tut",
    tutTargetDirectory := root.base / "docs",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "macro-compat" % Versions.macrocompat % "tut",
      "org.scala-lang" % "scala-compiler" % scalaVersion.value % "tut",
      compilerPlugin("org.scalamacros" % "paradise" % Versions.macroParadise cross CrossVersion.full),
      "com.outworkers" %% "util-samplers" % Versions.util % "tut",
      "io.circe" %% "circe-parser" % Versions.circe % "tut",
      "io.circe" %% "circe-generic" % Versions.circe % "tut",
      "org.scalatest" %% "scalatest" % Versions.scalatest % "tut"
    )
  ).dependsOn(
    phantomDsl,
    phantomJdk8,
    phantomExample,
    phantomConnectors,
    phantomFinagle,
    phantomStreams,
    phantomThrift
  ).enablePlugins(TutPlugin, CrossPerProjectPlugin)
