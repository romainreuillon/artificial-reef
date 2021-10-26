val scala3Version = "3.0.2"



lazy val root = project
  .in(file("."))
  .settings(
    name := "artificial-reef",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies += "org.apache.commons" % "commons-math3" % "3.6.1"
    //libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"
  )



enablePlugins(SbtOsgi)

osgiSettings

OsgiKeys.exportPackage := Seq("reef.*;-split-package:=merge-first")
OsgiKeys.importPackage := Seq("*;resolution:=optional")
OsgiKeys.privatePackage := Seq("!scala.*,!java.*,!META-INF.*.RSA,!META-INF.*.SF,!META-INF.*.DSA,META-INF.services.*,META-INF.*,*")
OsgiKeys.requireCapability := """osgi.ee;filter:="(&(osgi.ee=JavaSE)(version=1.8))""""

