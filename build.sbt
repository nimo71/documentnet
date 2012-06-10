name := "DocumentNet"

version := "0.0.1"

scalaVersion := "2.9.1"

libraryDependencies ++= Seq(
    "org.specs2" %% "specs2" % "1.11" % "test",
	"org.clapper" %% "grizzled-slf4j" % "0.6.6",
	"ch.qos.logback" % "logback-core" % "0.9.24" % "compile",
	"ch.qos.logback" % "logback-classic" % "0.9.24" % "compile",
	"org.slf4j" % "log4j-over-slf4j" % "1.6.1"
	//"com.weiglewilczek.slf4s" %% "slf4s" % "1.0.7",
	//"org.slf4j" % "slf4j-simple" % "1.6.2"
)
  
// Read here for optional dependencies: 
// http://etorreborre.github.com/specs2/guide/org.specs2.guide.Runners.html#Dependencies

resolvers ++= Seq("snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
                    "releases"  at "http://oss.sonatype.org/content/repositories/releases")
 