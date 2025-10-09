/**
 * @author damola
 *
 */
package net.mabogunje.yorudi

import io._
import scala.io.Codec
import java.nio.charset.CodingErrorAction

/**
 */
object Yorudi extends FileParser {
  val usage = "Usage: yorudi [--dict=cms|gpt|names|sample|] [-s (strict) | -g (glossary) | -d (derivative)] [word] [--fmt=plain|xml|json]"
    
  val dictionaries = Map[String, String](
    ("cms", "dicts/cms.en.yor"),
    ("gpt", "dicts/gpt.en.yor"),
    ("names", "dicts/names.en.yor"),
    ("sample", "dicts/sample.en.yor")
  )
   
  val printers = Map[String, YorudiWriter](
    ("plain", new CommandLineWriter()),
    ("xml", new XmlWriter()),
    ("json", new JsonWriter()))
   
  type OptionMap = Map[Symbol, Any]
        
  def parseOptions(map:OptionMap, list:List[String]):OptionMap = {
    def isSwitch(s:String) = (s.charAt(0) == '-')
        
    list match {
      case Nil => map
      case "--dict" :: value :: tail => parseOptions(map ++ Map('dict -> value), tail)
      case "--fmt" :: value :: tail => parseOptions(map ++ Map('format -> value), tail)
      case string :: opt :: tail if (isSwitch(string)) => {
        string match {
          case "-s" => parseOptions(map ++ Map('mode -> "strict"), list.tail)
          case "-g" => parseOptions(map ++ Map('mode -> "glossary"), list.tail)
          case "-d" => parseOptions(map ++ Map('mode -> "derivative"), list.tail)
          case _ => println("Invalid option: " + string); println(usage); sys.exit
        }
      }
      case option :: tail => parseOptions(map ++ Map('word -> option), tail)
    }
  }

  def main(args: Array[String]) {
      if (args.isEmpty) println (usage)
      val arguments = args.toList
            
      val options = parseOptions(Map(), arguments)
      
      if(options.isEmpty) {
        sys.exit
      }
      
      val showHelp = options.get('help).getOrElse(false)
      val dictKey = options.get('dict).get.toString
      
      if(!dictionaries.contains(dictKey)) {
        println("Unknown dictionary: " + dictKey)
        sys.exit
      }

      val dictFile = dictionaries(dictKey)
      val (index, lines) = indexFile(dictFile)
      val dict = IndexedDictionary(index, lines)
      val word = options.get('word).getOrElse("")
      var mode = options.get('mode).getOrElse("dictionary")
      var outputType = options.get('format).getOrElse("plain")
      var results = YorubaDictionary()
      var printer:YorudiWriter = if(printers.keys.exists(_ == outputType.toString)) printers(outputType.toString) else printers("plain")
      	
      mode match {
        case "glossary" => results = dict.lookupRelated(word)
        case "derivative" => results = dict.lookupDerivatives(word)
        case "strict" => results = dict.strictLookup(word)
        case _ => results = dict.lookup(word)
      }
      
      println(printer.writeGlossary(results))
  }
}
