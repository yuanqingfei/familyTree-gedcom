package gedcom

import org.gedcom4j.model.{Family, Individual, IndividualReference, StringWithCustomFacts}
import org.gedcom4j.parser.GedcomParser

import collection.JavaConverters._

object MyGedcomParser {
  def main(args: Array[String]): Unit = {
    val gp = new GedcomParser
    gp.load("gedcom/resources/firstDirect.ged")
    //    println("ERROR: " + gp.getErrors)
    //    println("WARNING: " + gp.getWarnings)

    val g = gp.getGedcom
    val families = g.getFamilies
    //    println(families)

    val famList = families.values().asScala
    val treeMap = scala.collection.mutable.SortedMap[Int, List[Family]]()
    val rootFam2 = families.get("@F0000@")

    printDesc3(1, rootFam2, famList, treeMap)

    //    println(treeMap)
    import java.io._
    val pw = new PrintWriter(new File("gedcom/resources/yuanGEDCOM.dot"))

    pw.write(printDotHead)
    for ((gen, families) <- treeMap) {
      pw.write(printDotFamilies(gen, families))
    }
    pw.write(printEnd)

    pw.close
  }

  def printDesc3(gen: Int, rootFam: Family, families: Iterable[Family], treeMap: scala.collection.mutable.SortedMap[Int, List[Family]]): Unit = {
    //        println("this is NO. " + gen + " genration: " + rootFam.getHusband.getIndividual.getFormattedName)
    treeMap.get(gen) match {
      case Some(famList) => {
        treeMap.put(gen, famList :+ rootFam)
      }
      case None => {
        treeMap.put(gen, List(rootFam))
      }
    }

    val genX = gen + 1
    for (child <- rootFam.getChildren.asScala) {
      val personId = child.getIndividual.getXref
      val childFam2 = families.filter(_.getHusband.getIndividual.getXref.equals(personId))

      if (childFam2.size != 0) {
        childFam2.map(printDesc3(genX, _, families, treeMap))
      }
    }
  }

  def printDotFamilies(gen:Int, families: List[Family]) = {
    var famliesStr = "/////////////////////// No. " + gen + " generation //////////////////// \n"
    families.map(fam => {
      famliesStr += ("//############ \n" + printDotFamily(fam))
    })
    famliesStr
  }

  def printDotFamily(family: Family) = {
    val husbandSignature = getSignature(family.getHusband.getIndividual)
    var wifeInd = family.getWife
    var wifeSignature = husbandSignature + "W";
    var wifeRender = wifeSignature + " [color=pink] \n"
    if (wifeInd != null) {
      wifeSignature = getSignature(wifeInd.getIndividual)
      wifeRender = getIndRender(wifeInd.getIndividual)
    }

    val marrigeNode = husbandSignature + "And" + wifeSignature
    val husbandAndWife = getIndRender(family.getHusband.getIndividual) + wifeRender + marrigeNode + " [shape = point] \n" + husbandSignature + " -> " + marrigeNode + " -> " + wifeSignature + "\n"
    val parentGraph = printSubHead(myTrim(family.getXref)) + husbandAndWife + printEnd

    val connectNode = marrigeNode + "Connect"
    val connectGraph = printSubHead(connectNode) + connectNode + " [shape = point, width = 0]\n" + printEnd
    val connect1 = marrigeNode + " -> " + connectNode + "\n"

    var childrenString = ""
    var connect2 = ""
    for (child: IndividualReference <- family.getChildren.asScala) {
      val realChild = child.getIndividual
      val childSignature = getSignature(realChild)
      val childRender = getIndRender(realChild)
      childrenString += childRender
      connect2 += (connectNode + " -> " + childSignature + "\n")
    }
    val childrenGraph = printSubHead(marrigeNode + "Children") + childrenString + printEnd

    parentGraph + connectGraph + connect1 + childrenGraph + connect2
  }

  def getSignature(person: Individual) = {
    myTrim(person.getXref)
  }

  def getIndRender(person: Individual) = {
    val signatureName = getSignature(person)
    if (person.getSex.getValue.equalsIgnoreCase("F")) {
      signatureName + " [label=\"" + person.getNames.get(0).getBasic + "\"]  [color=pink] \n"
    } else {
      signatureName + " [label=\"" + person.getNames.get(0).getBasic + "\"] \n"
    }
  }

  def printSubHead(graphName: String) = {
    "subgraph " + graphName + " {\nrank = same\n"
  }

  def printDotHead = {
    """
      |digraph {
      |    //splines = ortho;
      |    //rankdir="LR";
      |    node [shape = box, color=blue];
      |    edge [dir = none];
    """.stripMargin
  }

  def printEnd = {
    "}\n"
  }

  def myTrim(name: String): String = {
    name.drop(1).dropRight(1)
  }

}