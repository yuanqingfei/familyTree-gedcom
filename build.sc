// build.sc
import mill._
import mill.scalalib._

object gedcom extends ScalaModule {
  def scalaVersion = "2.12.4"

  override def ivyDeps = Agg(
    ivy"org.gedcom4j:gedcom4j:4.0.1",
    ivy"org.familysearch.gedcom:gedcom:1.8.0"
  )

  def mainClass = Some("gedcom.MyGedcomParser")
//  def mainClass = Some("gedcom.SecondParser")

}
