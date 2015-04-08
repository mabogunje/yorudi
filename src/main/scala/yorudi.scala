/**
 * @author damola
 *
 */
import io._
import scala.io.Codec
import java.nio.charset.CodingErrorAction

/**
 *
 */
object Yorudi extends FileParser {
    val usage = """
      Usage: yorudi [--dict=dictionary] [-s (strict) | -g (glossary)] [word]
"""
   val dictionaries = Map(
       ("cms", "src/main/resources/dicts/cms.en.yor"),
       ("sample", "src/main/resources/dicts/sample.yor"))
        
  def main(args: Array[String]) {
      if (args.isEmpty) println (usage)
      val arguments = args.toList
      type OptionMap = Map[Symbol, Any]
      
      def parseOptions(map:OptionMap, list:List[String]):OptionMap = {
        def isSwitch(s:String) = (s.charAt(0) == '-')
        
        list match {
          case Nil => map
          case "--dict" :: value :: tail => parseOptions(map ++ Map('dict -> value), tail)
          case string :: opt :: tail if (isSwitch(string)) => {
            string match {
              case "-s" => parseOptions(map ++ Map('lookup -> "strict"), list.tail)
              case "-g" => parseOptions(map ++ Map('mode -> "glossary"), list.tail)
              case _ => println("Invalid option: " + string); println(usage); exit(1)
            }
          }
          case option :: tail => parseOptions(map ++ Map('word -> option), tail)
        }
      }
      
      val options = parseOptions(Map(), arguments)
      
      if(options.isEmpty) {
        exit(1)
      }
      val showHelp = options.get('help).getOrElse(false)
      val dictKey = options.get('dict).get.toString
      
      if(!dictionaries.contains(dictKey)) {
        println("Unknown dictionary: " + dictKey)
        exit(1)
      }
      else {
        val dict = parse(dictionaries(dictKey))
        val word = options.get('word).getOrElse("")
        var searchType = options.get('lookup).getOrElse("default")
        var mode = options.get('mode).getOrElse("dictionary")
        var results = YorubaDictionary()
        var printer = CommandLineWriter()
      	
        if(mode == "glossary") {
          results = dict.lookupRelated(word)
        } else {
          results = if (searchType.toString == "strict") dict.strictLookup(word) else dict.lookup(word)
        }
      	
      	println(printer.writeGlossary(results))      
     }
  }
}