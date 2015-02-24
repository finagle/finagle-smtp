lazy val root = project.in(file("."))
  .settings( 
    moduleName := "finagle-smtp",
    organization := "io.github.finagle",
    version := "0.1.0",
    scalaVersion := "2.11.5",
    crossScalaVersions := Seq("2.10.4", "2.11.5"),
    scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature"),
    resolvers += "Twitter's Repository" at "http://maven.twttr.com/",
    libraryDependencies ++= Seq(
      "com.twitter" %% "finagle-mux" % "6.24.0",
      "com.twitter" %% "util-codec" % "6.23.0",
      "com.twitter" %% "util-logging" % "6.23.0",
      "junit" % "junit" % "4.12" % "test",
      "org.mockito" % "mockito-all" % "1.10.19" % "test",
      "org.scalatest" %% "scalatest" % "2.2.3" % "test"
    )
  )
  .settings(publishSettings: _*)
  .settings(site.settings: _*)
  .settings(site.includeScaladoc("docs"): _*)
  .settings(ghpages.settings: _*)
  .settings(
    git.remoteRepo := "git@github.com:finagle/finagle-smtp.git"
  )

lazy val publishSettings = Seq(
  publishMavenStyle := true,
  publishArtifact := true,
  useGpg := true,
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases"  at nexus + "service/local/staging/deploy/maven2")
  },
  publishArtifact in Test := false,
  licenses := Seq("Apache 2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
  homepage := Some(url("https://github.com/finagle/finagle-smtp")),
  pomExtra := (
    <scm>
      <url>git://github.com/finagle/finagle-smtp.git</url>
      <connection>scm:git://github.com/finagle/finagle-smtp.git</connection>
    </scm>
    <developers>
      <developer>
        <id>suncelesta</id>
        <name>Valeria Dymbitskaya</name>
      </developer>
    </developers>
  )
)
