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
      
  def main(args: Array[String]) {
      if (args.isEmpty) println (usage)
      val arguments = args.toList
      type OptionMap = Map[Symbol, Any]
      
      def parseOptions(map:OptionMap, list:List[String]):OptionMap = {
        def isSwitch(s:String) = (s.charAt(0) == '-')
        
        list match {
          case string :: opt :: tail if (isSwitch(string)) => {
            string match {
              case "-h" => println(usage); exit(1)
              case "-d" => parseOptions(map ++ Map('dict -> opt), tail)
              case _ => println("Invalid option: " + string); exit(1)
            }
          }
          case option :: tail => parseOptions(map ++ Map('word -> option), tail)
          case Nil => map
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
      	var results = dict.lookup(word)
        
      	println(results.size + " entries found for: " + word )
      	for (entry <- results) {
      	  println(entry._1.word.toYoruba)
      	  
      	  for(meaning <- entry._2) {
      	    println("- " + meaning.description + " (" + meaning.language + ")");
      	  }
      	  
      	  println("\n")
      	}
      }
  }
}