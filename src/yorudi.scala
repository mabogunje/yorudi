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
object Yorudi extends GrammarParser {
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
      
      def isComment(line:String) = (line.head == '#')
      val options = parseOptions(Map(), arguments)
      val codec = Codec.UTF8
      
      val file = Source.fromFile(options.get('path).get.toString)(codec)
      val lines = file.getLines
      val dict = Map[WordEntry, List[Meaning]]()
      
      while (lines.hasNext) {
        var ln = lines.next
        if (!ln.isEmpty)
          if (!isComment(ln)) 
            println(parse(wordEntry, ln)) 
      }
      

  }
}