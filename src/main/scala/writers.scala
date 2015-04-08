import scala.xml._

/**
 * Writer Interface - All dictionary writers must implement this interface
 */
trait YorudiWriter {
  def writeWord(entry:WordEntry): String
  def writeDecomposition(entry:WordEntry): String
  def writeTranslation(translation:Meaning): String
  def writeDefinition(definition:(WordEntry, List[Meaning])): String
  def writeGlossary(dictionary:YorubaDictionary): String
}

class CommandLineWriter() extends YorudiWriter {
  def writeWord(entry:WordEntry) = entry.word.toYoruba
  def writeDecomposition(entry:WordEntry) = s"[ ${entry.word.decomposition mkString " . "} ]"
  def writeTranslation(translation:Meaning) = s"- ${translation.description} {${translation.language}}"
 
  def writeDefinition(definition:(WordEntry, List[Meaning])) = {
    var output = new StringBuilder
    output ++= s"${writeWord(definition._1)} ${writeDecomposition(definition._1)}\n"
    
    for(meaning <- definition._2) {
      output ++= writeTranslation(meaning).toString()
      output ++= "\n"
    }
    
    output.toString
  }
  
  def writeGlossary(dictionary:YorubaDictionary) = {
    var output = new StringBuilder
    output ++= s"${dictionary.size} definition(s) found\n"
    
    for(definition <- dictionary) {
      output ++= writeDefinition(definition).toString
      output ++= "\n"
    }
    
    output.toString
  }
}

/*
case class XmlWriter() extends YorudiWriter {
  def writeWord(entry:WordEntry) = {
    <word>{writeDecomposition(entry)}</word>% Attribute(None, "name", Text(entry.word.spelling), Null) 
  }
  def writeDecomposition(entry:WordEntry) = {
    var rootless = entry.word.decomposition diff entry.word.root.toYoruba
    
    <decomposition>
      <root>entry.word.root</root>
      {rootless.map(term => <term>{term.toYoruba}</term>)}
    </decomposition>
  }
  def writeTranslation(translation:Meaning) = "To Do"
  def writeDefinition(definition:(WordEntry, List[Meaning])) = "To Do"
  def writeGlossary(dictionary:YorubaDictionary) = "To Do"

}
*/