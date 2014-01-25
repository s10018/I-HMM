
import AssemblyKeys._

name := "PL-MRF"

version := "0.0.1"

libraryDependencies  ++= Seq(
  // other dependencies here
  // pick and choose:
  "org.scalanlp" % "breeze_2.10" % "0.5.2"
)

resolvers ++= Seq(
  // other resolvers here
  // if you want to use snapshot builds (currently 0.6-SNAPSHOT), use this.
  "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/releases/"
)

assemblySettings


