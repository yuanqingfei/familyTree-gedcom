package gedcom

import java.io.File

import org.folg.gedcom.parser.ModelParser
import org.folg.gedcom.model.Person
import org.folg.gedcom.tools.GedcomAnalyzer

import collection.JavaConverters._

object SecondParser {

  def main(args: Array[String]): Unit ={

    val f = new File("gedcom/resources/firstDirect.ged")
    val mp = new ModelParser
    val gc = mp.parseGedcom(f)

    gc.getPeople.asScala.map(people => {println(people.getNames.get(0).getDisplayValue + " " + people.getId)})

    println("///////////////////")

    val analyzer = new GedcomAnalyzer
    analyzer.analyzeGedcom(new File("gedcom/resources/firstDirect.ged"))
    analyzer.getErrors.getKeys.asScala.map(key => {println(key)})
    analyzer.getWarnings.getKeys.asScala.map(key => {println(key)})
    println(analyzer.getWarnings.size())

    gc.createIndexes
    val p = gc.getPerson("I0049")

    println(p.getNames.get(0).getDisplayValue)
  }

}
