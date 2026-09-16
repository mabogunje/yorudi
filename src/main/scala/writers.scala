/**
 * @author damola
 *
 */
package net.mabogunje.yorudi

import scala.xml._
import org.json4s._

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
  def writeTranslation(translation:Meaning): String = s"- ${translation.description} (${translation.language})"
 
  def writeDefinition(definition:(WordEntry, List[Meaning])): String = {
    val output = new StringBuilder
    output ++= s"${writeWord(definition._1)} ${writeDecomposition(definition._1)}\n"
    
    for(meaning <- definition._2) {
      output ++= writeTranslation(meaning).toString()
      output ++= "\n"
    }
    
    output.toString
  }
  
  def writeGlossary(dictionary:YorubaDictionary): String = {
    val output = new StringBuilder
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
    val xml = <word>{writeDecomposition(entry)}</word> % Attribute(None, "spelling", Text(entry.word.spelling), Null)
    pretty(xml)
  }
  
  def writeDecomposition(entry:WordEntry): xml.Elem = {
    val xml = <decomposition>{entry.word.decomposition map(term =>
      if(term.properties.contains(Root)) <root>{term}</root> 
      else <term>{term}</term>
    )}</decomposition>
    
    pretty(xml)
  }
  
  def writeTranslation(translation:Meaning): xml.Elem = {
    val xml = <meaning>{translation.description}</meaning> % Attribute(None, "xml:language", Text(translation.language.toString), Null)
    pretty(xml)
  }
  
  def writeDefinition(definition:(WordEntry, List[Meaning])): xml.Elem = {
    val xml = <definition>{writeWord(definition._1)} {definition._2 map(meaning => writeTranslation(meaning))}</definition>
    pretty(xml)
  }
  
  def writeGlossary(dictionary:YorubaDictionary): xml.Elem = {
    val xml = <yorudi>{dictionary map(definition => writeDefinition(definition))}</yorudi>% Attribute(None, "wordCount", Text(dictionary.size.toString), Null)
    pretty(xml)
  }
}

case class JsonWriter() extends YorudiWriter {
  def writeWord(entry:WordEntry): JValue = {
    JString(entry.word.toYoruba)
  }
  
  def writeDecomposition(entry:WordEntry): JValue = {
    JArray(entry.word.decomposition.map { term =>
      JObject(
        "spelling" -> JString(term.toYoruba),
        "root" -> JBool(term.properties.contains(Root))
      )
    }.toList)
  }

  def writeTranslation(translation:Meaning): JValue = {
    JObject(
      "description" -> JString(translation.description),
      "language" -> JString(translation.language)
    )
  }

  def writeDefinition(definition:(WordEntry, List[Meaning])): JValue = {
    val (wordEntry, meanings) = definition
    JObject(
      "definition" -> writeWord(wordEntry),
      "decomposition" -> writeDecomposition(wordEntry),
      "meanings" -> JArray(meanings.map(writeTranslation))
    )
  }

  def writeGlossary(dictionary:YorubaDictionary): JValue = {
    JArray(dictionary.map(writeDefinition).toList)
  }
}
