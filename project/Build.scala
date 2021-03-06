import sbt._
import Keys._
import play.Project._

object ApplicationBuild extends Build {

  val appName         = "blurb"
  val appVersion      = "1.0-SNAPSHOT"

  val appDependencies = Seq(
    // Add your project dependencies here,
    //jdbc,
    //anorm,
    "securesocial" %% "securesocial" % "master-SNAPSHOT",
    "com.clever-age" % "play2-elasticsearch" % "0.6-SNAPSHOT",
    "org.mongodb" % "mongo-java-driver" % "2.11.2",
    //"uk.co.panaxiom" %% "play-jongo" % "0.4",
    //"com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.2.1",
    "org.reactivemongo" %% "play2-reactivemongo" % "0.9-SNAPSHOT",
    "io.backchat.inflector" %% "scala-inflector" % "1.3.5",
    "org.ocpsoft.prettytime" % "prettytime" % "3.0.2.Final"
    //"com.typesafe.slick" %% "slick" % "1.0.1"
  )

  val main = play.Project(appName, appVersion, appDependencies).settings(
    // Add your own project settings here
    offline := false,
    resolvers += Resolver.url("sbt-plugin-snapshots", new URL("http://repo.scala-sbt.org/scalasbt/sbt-plugin-snapshots/"))(Resolver.ivyStylePatterns),
    resolvers += Resolver.url("play-plugin-releases", new URL("http://repo.scala-sbt.org/scalasbt/sbt-plugin-releases/"))(Resolver.ivyStylePatterns),
    resolvers += Resolver.url("My GitHub Play Repository", url("http://alexanderjarvis.github.com/releases/"))(Resolver.ivyStylePatterns),
    resolvers += "Sonatype Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"
  )
}
