import sbt._

object CoincheBuild extends Build {
  lazy val root = Project("root",file(".")) dependsOn(coinche)
  lazy val coinche = RootProject(uri("git://github.com/martin1keogh/Coinche.git"))
}
