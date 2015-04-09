import scala.xml._

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
    output ++= s"${dictionary.size} definition(s) found\n"
    
    for(definition <- dictionary) {
      output ++= writeDefinition(definition).toString
      output ++= "\n"
    }
    
    output.toString
  }
}


case class XmlWriter() extends YorudiWriter {
  def writeWord(entry:WordEntry): xml.Elem = {
    <word>{writeDecomposition(entry)}</word> % Attribute(None, "spelling", Text(entry.word.spelling), Null) 
  }
  
  def writeDecomposition(entry:WordEntry): xml.Elem = {
    val (before, atAndAfter) = entry.word.decomposition.toList span (term => term != entry.word.root)
    var rootless = before ::: atAndAfter.drop(1)
    
    if(rootless.isEmpty) {
      <decomposition><root>{entry.word.root}</root></decomposition>
    } else {
      <decomposition><root>{entry.word.root}</root>{rootless map(term => <term>{term.toYoruba}</term>)}</decomposition>
    }   
  }
  
  def writeTranslation(translation:Meaning): xml.Elem = {
    <meaning>{translation.description}</meaning> % Attribute(None, "xml:lang", Text(translation.language.toString), Null)
  }
  
  def writeDefinition(definition:(WordEntry, List[Meaning])): xml.Elem = {
    <definition>{writeWord(definition._1)} {definition._2 map(meaning => writeTranslation(meaning))}</definition>
  }
  
  def writeGlossary(dictionary:YorubaDictionary): xml.Elem = {
    <yorudi>{dictionary map(definition => writeDefinition(definition))}</yorudi>% Attribute(None, "wordCount", Text(dictionary.size.toString), Null)
  }
}