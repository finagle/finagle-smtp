lazy val root = project.in(file("."))
  .settings( 
    moduleName := "finagle-smtp",
    organization := "io.github.finagle",
    version := "0.1.0",
    scalaVersion := "2.11.7",
    crossScalaVersions := Seq("2.10.5", "2.11.7"),
    scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature"),
    resolvers += "Twitter's Repository" at "https://maven.twttr.com/",
    libraryDependencies ++= Seq(
      "com.twitter" %% "finagle-mux" % "6.29.0",
      "com.twitter" %% "util-codec" % "6.28.0",
      "com.twitter" %% "util-logging" % "6.28.0",
      "junit" % "junit" % "4.12" % "it,test",
      "org.mockito" % "mockito-all" % "1.10.19" % "it,test",
      "org.scalatest" %% "scalatest" % "2.2.5" % "it,test"
    )
  )
  .settings(publishSettings ++ site.settings ++ ghpages.settings)
  .settings(
    site.includeScaladoc("docs"),
    git.remoteRepo := "git@github.com:finagle/finagle-smtp.git"
  )
  .configs(IntegrationTest)
  .settings(Defaults.itSettings: _*)

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
  autoAPIMappings := true,
  apiURL := Some(url("https://finagle.github.io/finagle-smtp/docs/")),
  scmInfo := Some(
    ScmInfo(
      url("https://github.com/finagle/finagle-smtp"),
      "scm:git:git@github.com:finagle/finagle-smtp.git"
    )
  ),
  pomExtra := (
    <developers>
      <developer>
        <id>suncelesta</id>
        <name>Valeria Dymbitskaya</name>
      </developer>
    </developers>
  )
)
