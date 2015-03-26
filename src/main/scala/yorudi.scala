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
      Usage: yorudi [-d] dictionary [word]
"""
   val dictionaries = Map(
       ("cms", "src/main/resources/dicts/cms.en.yor"),
       ("sample", "src/main/resources/dicts/sample.yor"))
   
  def print(results: YorubaDictionary) = {
    for (entry <- results) {
      println(entry._1.word.toYoruba + " [ " + (entry._1.word.decomposition mkString " . ") + " ]")
      	  
        for(meaning <- entry._2) {
          println("- " + meaning.description + " (" + meaning.language + ")");
        }
      	  
        println("\n")
    }
  }
      
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
              case "-h" => println(usage); exit(1)
              case "-s" => parseOptions(map ++ Map('lookup -> "strict"), list.tail)
              case "-g" => parseOptions(map ++ Map('mode -> "glossary"), list.tail)
              case _ => println("Invalid option: " + string); exit(1)
            }
          }
          case option :: tail => parseOptions(map ++ Map('word -> option), tail)
        }
      }
      
      val options = parseOptions(Map(), arguments)
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
        var info = "number of results found"
        var results = YorubaDictionary()
      	
        if(mode == "glossary") {
          results = dict.lookupRelated(word)
          info = results.size + " word(s) found related to: " + word
        } else {
          results = if (searchType.toString == "strict") dict.strictLookup(word) else dict.lookup(word)
          info = results.size + " definition(s) found for: " + word
        }
      	
      	println(info)
      	print(results)      
     }
  }
}