
import AssemblyKeys._
import com.typesafe.sbt.SbtStartScript

name := "PL-MRF"

version := "0.0.1"

mainClass in Compile := Some("scala.plmrf.Main")

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

jarName in assembly := "PL-MRF.jar"

mainClass in assembly := Some("scala.plmrf.Main")

assemblySettings

seq(SbtStartScript.startScriptForClassesSettings: _*)
