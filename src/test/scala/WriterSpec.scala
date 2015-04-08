import YorubaImplicits._
import org.scalatest.FlatSpec

class WriterSpec extends FlatSpec{
  var writer:YorudiWriter = new CommandLineWriter()
  
  "The Command Line Writer" can "write words correctly" in {
    var entry = new WordEntry(Word("de", List("de" as Root)), Map())
    var output = writer.writeWord(entry)
        
    assert(output.toString == "de")
  }
  
  it can "write decompositions correctly" in {
    var entry = new WordEntry(Word("ade", List("a", "de" as Root)), Map())
    var output = writer.writeDecomposition(entry)
    
    assert(output.toString == "[ a . de ]")
  }
  
  it can "write translations correctly" in {
    var translation = Translation("crown", "en")
    var output = writer.writeTranslation(translation)
    
    assert(output.toString() == "- crown {en}")
  }
}