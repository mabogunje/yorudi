/**
 * @author damola
 *
 */
package net.mabogunje.yorudi

import YorubaImplicits._
import scala.xml.XML
import org.scalatest.flatspec.AnyFlatSpec

class CmdlWriterSpec extends AnyFlatSpec {
  val writer:YorudiWriter = new CommandLineWriter()
  
  "The Command Line Writer" can "write words correctly" in {
    val entry = new WordEntry(Word("de", List("de" as Root)), Map())
    val output = writer.writeWord(entry)
    val expected = "de"
        
    assert(output.toString == expected)
  }
  
  it can "write decompositions correctly" in {
    val entry = new WordEntry(Word("ade", List("a", "de" as Root)), Map())
    val output = writer.writeDecomposition(entry)
    val expected = "[ a . de ]"
    
    assert(output.toString == expected)
  }
  
  it can "write translations correctly" in {
    val translation = Translation("crown", "en-NG")
    val output = writer.writeTranslation(translation)
    val expected = s"- ${translation.description} (${translation.language})"

    assert(output.toString == expected)
  }
}

class XmlWriterSpec extends AnyFlatSpec {
  val writer:YorudiWriter = new XmlWriter()
  val printer = new xml.PrettyPrinter(80, 2)
  def format(element: xml.Elem) = XML.loadString(printer format element)
  
  "The Xml writer" can "write words correctly" in {
    val entry = new WordEntry(Word("gbogbo", List("gbo" as Root, "gbo")), Map())
    val output = writer.writeWord(entry)
    val expected = <word spelling="gbogbo"><decomposition><root>gbo</root><term>gbo</term></decomposition></word>
    
    assert(output.toString == format(expected).toString)
  }
  
  it can "write decompositions correctly" in {
    val entry = new WordEntry(Word("gbogbo", List("gbo" as Root, "gbo")), Map())
    val output = writer.writeDecomposition(entry)
    val expected = <decomposition><root>gbo</root><term>gbo</term></decomposition>
      
    assert(output.toString == format(expected).toString)
  }
  
  it can "write translations correctly" in {
    val translation = Translation("plenty", "en-NG")
    val output = writer.writeTranslation(translation)
    val expected = <meaning xml:language={translation.language.toString()}>{translation.description}</meaning>
    
    assert(output.toString == format(expected).toString)
  }
}

import org.json4s._

class JsonWriterSpec extends AnyFlatSpec {
  val writer:JsonWriter = new JsonWriter()
  
  "The JSON writer" can "write words correctly" in {
    val entry = new WordEntry(Word("gbogbo", List("gbo" as Root, "gbo")), Map())
    val output = writer.writeWord(entry)
    val expected = JString("gbogbo")

    assert(output == expected)
  }

  it can "write decompositions correctly" in {
    val entry = new WordEntry(Word("gbogbo", List("gbo" as Root, "gbo")), Map())
    val output = writer.writeDecomposition(entry)
    val expected = JArray(List(
      JObject("spelling" -> JString("gbo"), "root" -> JBool(true)),
      JObject("spelling" -> JString("gbo"), "root" -> JBool(false))
    ))

    assert(output == expected)
  }

  it can "write translations correctly" in {
    val translation = Translation("plenty", "en-NG")
    val output = writer.writeTranslation(translation)
    val expected = JObject(
      "description" -> JString("plenty"),
      "language" -> JString("en-NG")
    )

    assert(output == expected)
  }

  it can "write definitions without leaking domain internals" in {
    val entry = new WordEntry(Word("gbogbo", List("gbo" as Root, "gbo")), Map("source" -> "test"))
    val translation = Translation("plenty", "en-NG")
    val output = writer.writeDefinition((entry, List(translation)))
    val expected = JObject(
      "definition" -> JString("gbogbo"),
      "decomposition" -> JArray(List(
        JObject("spelling" -> JString("gbo"), "root" -> JBool(true)),
        JObject("spelling" -> JString("gbo"), "root" -> JBool(false))
      )),
      "meanings" -> JArray(List(
        JObject(
          "description" -> JString("plenty"),
          "language" -> JString("en-NG")
        )
      ))
    )

    assert(output == expected)
    assert(output.findField(_._1 == "attributes").isEmpty)
    assert(output.findField(_._1 == "properties").isEmpty)
  }
}
