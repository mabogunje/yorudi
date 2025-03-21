import YorubaImplicits._
import scala.xml.PrettyPrinter
import scala.xml.XML
import org.scalatest.FlatSpec
import scala.util.parsing.json._


class CmdlWriterSpec extends FlatSpec {
  var writer:YorudiWriter = new CommandLineWriter()
  
  "The Command Line Writer" can "write words correctly" in {
    var entry = new WordEntry(Word("de", List("de" as Root)), Map())
    var output = writer.writeWord(entry)
    var expected = "de"
        
    assert(output.toString == expected)
  }
  
  it can "write decompositions correctly" in {
    var entry = new WordEntry(Word("ade", List("a", "de" as Root)), Map())
    var output = writer.writeDecomposition(entry)
    var expected = "[ a . de ]"
    
    assert(output.toString == expected)
  }
  
  it can "write translations correctly" in {
    var translation = Translation("crown", "en")
    var output = writer.writeTranslation(translation)
    var expected = "- crown {en}"
    
    assert(output.toString == expected)
  }
}

class XmlWriterSpec extends FlatSpec {
  var writer:YorudiWriter = new XmlWriter()
  var printer = new xml.PrettyPrinter(80, 2)
  def format(element: xml.Elem) = XML.loadString(printer format element)
  
  "The Xml writer" can "write words correctly" in {
    var entry = new WordEntry(Word("gbogbo", List("gbo" as Root, "gbo")), Map())
    var output = writer.writeWord(entry)
    var expected = <word spelling="gbogbo"><decomposition><root>gbo</root><term>gbo</term></decomposition></word>
    
    assert(output.toString == format(expected).toString)
  }
  
  it can "write decompositions correctly" in {
    var entry = new WordEntry(Word("gbogbo", List("gbo" as Root, "gbo")), Map())
    var output = writer.writeDecomposition(entry)
    var expected = <decomposition><root>gbo</root><term>gbo</term></decomposition>
      
    assert(output.toString == format(expected).toString)
  }
  
  it can "write translations correctly" in {
    var translation = Translation("plenty", "en")
    var output = writer.writeTranslation(translation)
    var expected = <meaning xml:lang="en">plenty</meaning>
    
    assert(output.toString == format(expected).toString)
  }
}

class JsonWriterSpec extends FlatSpec {
  var writer:YorudiWriter = new JsonWriter()
  
  "The JSON writer" can "write words correctly" in {
    var entry = new WordEntry(Word("gbogbo", List("gbo" as Root, "gbo")), Map())
    var output = writer.writeWord(entry)
    var expected = """{"spelling" : "gbogbo", "root" : "gbo", "decomposition" : ["gbo", "gbo"]}"""

    assert(output.toString == expected)
  }

  it can "write decompositions correctly" in {
    var entry = new WordEntry(Word("gbogbo", List("gbo" as Root, "gbo")), Map())
    var output = writer.writeDecomposition(entry)
    var expected = """["gbo", "gbo"]"""

    assert(output.toString == expected)
  }

  it can "write translations correctly" in {
    var translation = Translation("plenty", "en")
    var output = writer.writeTranslation(translation)
    var expected = """{"description" : "plenty", "language" : "en"}"""

    assert(output.toString == expected)
  }
}
