import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

name := "teseo"

inThisBuild(
  List(
    scalaVersion := "3.3.1",
    organization := "io.github.pablf",
    homepage     := Some(url("https://github.com/pablf/teseo")),
    licenses     := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
    developers   := List(
      Developer(
        "pablf",
        "Pablo Femenía",
        "pablofemenia@proton.me",
        url("https://github.com/pablf/teseo")
      )
    )
  )
)
fork in Test := true
scalacOptions += "-Ywarn-unused"

ThisBuild / sonatypeCredentialHost := "s01.oss.sonatype.org"
sonatypeRepository := "https://s01.oss.sonatype.org/service/local"

addCommandAlias("fmt", "; all scalafmtSbt scalafmtAll")
addCommandAlias("check", "; scalafmtSbtCheck; scalafmtCheckAll")

addCommandAlias(
  "testJVM",
  ";core/test"
)

val scalatestVersion = "3.2.17"

def mkSettings(platform: String, conf: String, baseDirectory: File) =
  for {
    platform <- List("shared", platform)
    result    = baseDirectory.getParentFile / platform.toLowerCase / "src" / conf / "scala"
    if result.exists
  } yield result

lazy val settings = Seq(
  Compile / unmanagedSourceDirectories ++= {
    mkSettings(
      crossProjectPlatform.value.identifier,
      "main",
      baseDirectory.value
    )
  },
  Test / unmanagedSourceDirectories ++= {
    mkSettings(
      crossProjectPlatform.value.identifier,
      "test",
      baseDirectory.value
    )
  }
)

lazy val root = project
  .in(file("."))
  .aggregate(teseo.jvm, teseo.native)

lazy val teseo = crossProject(JVMPlatform, NativePlatform, JSPlatform)
  .in(file("teseo"))
  .settings(name := "teseo")
  .settings(settings)
  .settings(libraryDependencies += "org.scalatest" %% "scalatest" % scalatestVersion % Test)
  .settings(scalacOptions += "-language:experimental.macros")
  .settings(scalacOptions += "-language:experimental")

lazy val examples = crossProject(JVMPlatform, NativePlatform, JSPlatform)
  .in(file("examples"))
  .settings(name := "teseo-examples")
  .settings(settings)
  .dependsOn(teseo)

lazy val docs = project
  .in(file("teseo-docs"))
  .dependsOn(teseo.jvm)
  .enablePlugins(MdocPlugin)

Global / onChangedBuildSource := ReloadOnSourceChanges
