
name := "codicography"

version := "1.0.1"

// must be at least 2.11 to use hmt_textmodel
scalaVersion := "2.12.8"

run / connectInput := true

resolvers += Resolver.jcenterRepo
resolvers += Resolver.bintrayRepo("neelsmith","maven")
resolvers += Resolver.bintrayRepo("eumaeus", "maven")
resolvers += sbt.Resolver.bintrayRepo("denigma", "denigma-releases")

Compile / run / fork := true


connectInput in run := true

javaOptions in run ++= Seq(
    "-Xms256M",
    "-Xmn16M",
    "-Xmx4G"
)

libraryDependencies ++=   Seq(
  "edu.holycross.shot.cite" %% "xcite" % "4.1.1",
  "edu.holycross.shot" %% "ohco2" % "10.13.2",
  "edu.holycross.shot" %% "scm" % "7.0.1",
  "edu.holycross.shot" %% "dse" % "5.2.2",
  "edu.holycross.shot" %% "citebinaryimage" % "3.1.0",
  "edu.holycross.shot" %% "citeobj" % "7.3.4",
  "edu.holycross.shot" %% "citerelations" % "2.5.2",
  "edu.holycross.shot" %% "cex" % "6.3.3",
  "edu.holycross.shot" %% "greek" % "2.3.3",
  "edu.furman.classics" %% "citewriter" % "1.2.2",
  "com.github.pathikrit" %% "better-files" % "3.8.0"
)

