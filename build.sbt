name := "vhdl_parser"

version := "1.0"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "ch.qos.logback" % "logback-classic" % "1.0.13",
  "org.slf4j" % "slf4j-api" % "1.7.13",
  "org.scalatest" %% "scalatest" % "3.0.0-M12" % "test"
)

unmanagedClasspath in(Compile, runMain) += baseDirectory.value / ""

antlr4Settings
antlr4PackageName in Antlr4 := Some("sg.edu.ntu.hchen")
antlr4GenListener in Antlr4 := true
antlr4GenVisitor in Antlr4 := true