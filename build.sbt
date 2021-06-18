val scala3Version = "3.0.1-RC1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "scala3-simple",
    scalacOptions ++= Seq("-Xmax-inlines", "256"),
    version := "0.1.0",
    scalaVersion := scala3Version,
    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",
    libraryDependencies += "dev.zio" % "izumi-reflect_3" % "1.1.3-RC1"
  )
