name := "SpinalTemplateSbt"
version := "1.0"
scalaVersion := "2.11.12"
val spinalVersion = "1.4.0"


lazy val root = (project in file("."))
  .settings(
    inThisBuild(List(
      organization := "com.github.spinalhdl",
      scalaVersion := "2.11.12",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "superproject",
    libraryDependencies ++= Seq(
      "com.github.spinalhdl" % "spinalhdl-core_2.11" % spinalVersion,
      "com.github.spinalhdl" % "spinalhdl-lib_2.11" % spinalVersion,
      compilerPlugin("com.github.spinalhdl" % "spinalhdl-idsl-plugin_2.11" % spinalVersion)
    )
  ).dependsOn(vexRiscv)

lazy val vexRiscv = RootProject(file("./libraries/VexRiscv"))

fork := true
EclipseKeys.withSource := true
