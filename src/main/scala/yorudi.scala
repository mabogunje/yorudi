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
      Usage: yorudi [-f] pathToFileorFolder
"""
      
  def main(args: Array[String]) {
      if (args.isEmpty) println (usage)
      val arguments = args.toList
      type OptionMap = Map[Symbol, Any]
      
      def parseOptions(map:OptionMap, list:List[String]):OptionMap = {
        def isSwitch(s:String) = (s.charAt(0) == '-')
        
        list match {
          case string :: opt :: tail if (isSwitch(string)) => {
            string match {
              case "-f" => parseOptions(map ++ Map('path -> opt), tail)
            }
          }
          case option :: tail => println("Unknown option " + option); exit(1)
          case Nil => map
        }
      }
      
      val options = parseOptions(Map(), arguments)
      val dict = parse(options.get('path).get.toString)
      
      var results = dict.lookupRelated("ba");
      //var results = dict.lookup("abanisise");
      
      for (entry <- results) println(entry._1 + " -> " + entry._2);
  }
}