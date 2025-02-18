import scala.xml._
import scala.util.parsing.json._
import scala.collection.mutable.ListBuffer

/**
 * Writer Interface - All dictionary writers must implement this interface
 */
trait YorudiWriter {
  def writeWord(entry:WordEntry): Any
  def writeDecomposition(entry:WordEntry): Any
  def writeTranslation(translation:Meaning): Any
  def writeDefinition(definition:(WordEntry, List[Meaning])): Any
  def writeGlossary(dictionary:YorubaDictionary): Any
}

class CommandLineWriter() extends YorudiWriter {
  def writeWord(entry:WordEntry): String = entry.word.toYoruba
  def writeDecomposition(entry:WordEntry): String = s"[ ${entry.word.decomposition mkString " . "} ]"
  def writeTranslation(translation:Meaning): String = s"- ${translation.description} {${translation.language}}"
 
  def writeDefinition(definition:(WordEntry, List[Meaning])): String = {
    var output = new StringBuilder
    output ++= s"${writeWord(definition._1)} ${writeDecomposition(definition._1)}\n"
    
    for(meaning <- definition._2) {
      output ++= writeTranslation(meaning).toString()
      output ++= "\n"
    }
    
    output.toString
  }
  
  def writeGlossary(dictionary:YorubaDictionary): String = {
    var output = new StringBuilder
    output ++= s"${dictionary.size} word(s) found\n"
    
    for(definition <- dictionary) {
      output ++= writeDefinition(definition).toString
      output ++= "\n"
    }
    
    output.toString
  }
}

case class XmlWriter() extends YorudiWriter {
  val printer = new PrettyPrinter(80, 2)
  
  def pretty(element: xml.Elem): xml.Elem = {
    XML.loadString(printer format element)
  }
  
  def writeWord(entry:WordEntry): xml.Elem = {
    var xml = <word>{writeDecomposition(entry)}</word> % Attribute(None, "spelling", Text(entry.word.spelling), Null)
    pretty(xml)
  }
  
  def writeDecomposition(entry:WordEntry): xml.Elem = {
    var xml = <decomposition>{entry.word.decomposition map(term => 
      if(term.properties.contains(Root)) <root>{term}</root> 
      else <term>{term}</term>
    )}</decomposition>
    
    pretty(xml)
  }
  
  def writeTranslation(translation:Meaning): xml.Elem = {
    var xml = <meaning>{translation.description}</meaning> % Attribute(None, "xml:lang", Text(translation.language.toString), Null)
    pretty(xml)
  }
  
  def writeDefinition(definition:(WordEntry, List[Meaning])): xml.Elem = {
    var xml = <definition>{writeWord(definition._1)} {definition._2 map(meaning => writeTranslation(meaning))}</definition>
    pretty(xml)
  }
  
  def writeGlossary(dictionary:YorubaDictionary): xml.Elem = {
    var xml = <yorudi>{dictionary map(definition => writeDefinition(definition))}</yorudi>% Attribute(None, "wordCount", Text(dictionary.size.toString), Null)
    pretty(xml)
  }
}

case class JsonWriter() extends YorudiWriter {
  
  def writeWord(entry:WordEntry): JSONObject = {
    // Convert the WordEntry to a Map[String, Any] so that it can be consumed by JSONObject
    var raw = Map[String, Any](
      "spelling" -> entry.word.toString,
      "properties" -> JSONArray(entry.word.properties.toList),
      "root" -> entry.word.root.toString,
      "isElided" -> entry.word.isElided,
      "isAssimilated" -> entry.word.isAssimilated,
      "decomposition" -> writeDecomposition(entry)
    )
    
    // Convert the Map to JSON
    var json = JSONObject(raw);
  
    return json
  }
  
  def writeDecomposition(entry:WordEntry): JSONArray = {
    var json = JSONArray(entry.word.decomposition.toList)

    return json
  }

  def writeTranslation(translation:Meaning): JSONObject = {
    var raw = Map[String, String](
      "description" -> translation.description,
      "language" -> translation.language.toString
    )

    var json = JSONObject(raw)

    return json
  }

  def writeDefinition(definition:(WordEntry, List[Meaning])): JSONObject = {
    var raw = Map[String, Any] (
      "definition" -> writeWord(definition._1),
      "meanings" -> JSONArray(definition._2.map(writeTranslation)) 
    )
    
    var json = JSONObject(raw)

    return json
  }

  def writeGlossary(dictionary:YorubaDictionary): JSONArray = {
    var dict = new ListBuffer[JSONObject]()

    for(definition <- dictionary) {
      var json = writeDefinition(definition)

      if(json != None) { dict += json } else { println("Unable to parse: " + definition.toString())}
    }

    var glossary = JSONArray(dict.toList)
    return glossary
  }
}
