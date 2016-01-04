import sbt._
import Keys._
import com.typesafe.sbt.pgp.PgpKeys._

val commonSettings = Seq(
  organization := "com.github.omidb",
  version := "0.1.0-SNAPSHOT",
  scalaVersion := "2.11.6",
  testFrameworks += new TestFramework("utest.runner.Framework"),
  resolvers += Resolver.sonatypeRepo("snapshots"),
  libraryDependencies ++= Seq(
    "com.lihaoyi" %%% "utest" % "0.3.1" % "test",
    "com.github.omidb" %%% "dgraph" % "0.1.0-SNAPSHOT",
    "com.lihaoyi" %%% "fastparse" % "0.2.1",
    "com.beachape" %% "enumeratum" % "1.3.2"
  )
)

def preventPublication(p: Project) =
  p.settings(
    publish :=(),
    publishLocal :=(),
    publishSigned :=(),
    publishLocalSigned :=(),
    publishArtifact := false,
    publishTo := Some(Resolver.file("Unused transient repository", target.value / "fakepublish")),
    packagedArtifacts := Map.empty)

lazy val nlputils = crossProject
  .settings(commonSettings: _*)
  .settings(
    name := "nlputils",
    scmInfo := Some(ScmInfo(
      url("https://github.com/omidb/nlputils"),
      "scm:git:git@github.com:omidb/nlputils.git",
      Some("scm:git:git@github.com:omidb/nlputils.git"))),
    publishMavenStyle := true,
    publishArtifact in Test := false,
    pomExtra :=
      <url>https://github.com/omidb/nlputils</url>
        <licenses>
          <license>
            <name>MIT license</name>
            <url>http://www.opensource.org/licenses/mit-license.php</url>
          </license>
        </licenses>
        <developers>
          <developer>
            <id>omidb</id>
            <name>Omid Bakhshandeh</name>
            <url>https://github.com/omidb</url>
          </developer>
        </developers>,
    pomIncludeRepository := { _ => false },
    publishTo := {
      val nexus = "https://oss.sonatype.org/"
      if (isSnapshot.value)
        Some("snapshots" at nexus + "content/repositories/snapshots")
      else
        Some("releases" at nexus + "service/local/staging/deploy/maven2")
    }
  ).jsSettings(
  scalaJSStage in Global := FastOptStage
).jvmSettings(
  libraryDependencies ++= Seq(
    "com.github.pathikrit" %% "better-files" % "2.13.0",
    "org.scalaj" %% "scalaj-http" % "2.2.0",
    "org.scala-lang.modules" %% "scala-xml" % "1.0.3"
  )
)

lazy val nlputilsJS = nlputils.js

lazy val nlputilsJVM = nlputils.jvm


lazy val root = preventPublication(project.in(file(".")))
  .settings()
  .aggregate(nlputilsJS, nlputilsJVM)
