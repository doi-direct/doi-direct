name := "doi-direct"

version := "0.0.1"

scalaVersion := "2.10.0"

resolvers ++= Seq(
	"Java.net Maven2 Repository" at "http://download.java.net/maven/2/",
	"Sonatype Nexus Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
	"Sonatype Nexus Releases" at "https://oss.sonatype.org/content/repositories/releases",
	"Scala Snapshots" at "http://scala-tools.org/repo-snapshots/",
	"twitter-repo" at "http://maven.twttr.com"
)

// Project dependencies
libraryDependencies ++= Seq(
	"com.twitter" %% "finagle-core" % "6.1.0",
	"com.twitter" %% "finagle-http" % "6.1.0",
	"org.apache.httpcomponents" % "httpclient" % "4.1.2"
)

// Test dependencies
libraryDependencies ++= Seq(
	"junit" % "junit" % "4.8" % "test",
	"org.scalatest" %% "scalatest" % "1.9.1" % "compile,test"
)

// Sometimes it's useful to see debugging out from the typer (e.g. to resolve slow compiles)
// scalacOptions += "-Ytyper-debug"

// 'sbt dependency-graph'
net.virtualvoid.sbt.graph.Plugin.graphSettings

seq(com.typesafe.startscript.StartScriptPlugin.startScriptForClassesSettings: _*)

