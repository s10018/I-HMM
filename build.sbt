
import AssemblyKeys._
import com.typesafe.sbt.SbtStartScript

name := "I-HMM"

version := "0.0.1"

mainClass in Compile := Some("scala.ihmm.Main")

scalacOptions ++= Seq( "-unchecked", "-deprecation" )

libraryDependencies  ++= Seq(
  // other dependencies here
  // pick and choose:
  "org.scalanlp" % "breeze_2.10" % "0.5.2"
)

libraryDependencies += "org.scalatest" %% "scalatest" % "2.0.M6-SNAP8" % "test"

resolvers ++= Seq(
  // other resolvers here
  // if you want to use snapshot builds (currently 0.6-SNAPSHOT), use this.
  "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/releases/"
)

jarName in assembly := "I-HMM.jar"

mainClass in assembly := Some("scala.ihmm.Main")

assemblySettings

seq(SbtStartScript.startScriptForClassesSettings: _*)
