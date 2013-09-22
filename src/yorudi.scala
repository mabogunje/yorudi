/**
 * @author damola
 *
 */
import io._
import scala.io.Codec
import scala.collection.JavaConversions
import java.nio.charset.CodingErrorAction

/**
 *
 */
object Yorudi extends FileParser {
  val name = "Yòrúdí Multilingual Yoruba Dictionary"
  val version = 0.1
  val usage = "usage: yorudi [--version] [--help] <command> [<args>]";
  val about = s""" $name
  $usage
  
  The most commonly used yorudi commands are:
  --lookup    Looks up the definition of a Yoruba word
  
  See yorudi <command> -h for more information on a specific command
    """;
  val src = "dicts/"
      
  def main(args: Array[String]) {
      if (args.isEmpty) println (about)
      val arguments = args.toList
      type OptionMap = Map[Symbol, Any]
      
      def parseOptions(map:OptionMap, list:List[String]):OptionMap = {
        def isSwitch(s:String) = (s.charAt(0) == '-')
        
        list match { 
          case Nil => map
          case "--version" :: tail => println(s"yorudi version $version"); exit(1)
          case "--help" :: tail => println(usage); exit(1)
          case "--lookup" :: opt :: tail if !isSwitch(opt) => parseOptions(map ++ Map('cmd -> "l", 'strict -> true, 'word -> opt), list.tail)
          case "--lookup" :: opt :: tail if isSwitch(opt) => parseOptions(map ++ Map('cmd -> "l", 'strict -> true), list.tail)
          case opt :: tail if isSwitch(opt) => {
            opt.drop(1).toList match {
              case char :: tail => char match {
                case '-' => println("Unknown command"); exit(1)
                case 'l' => parseOptions(map ++ Map('cmd -> "l", 'strict -> true), (List("-" + tail.mkString("")) ::: list.tail))
                case 'a' => parseOptions(map ++ Map('strict -> false), (List("-" + tail.mkString("")) ::: list.tail))
                case 'r' => parseOptions(map ++ Map('root -> true), (List("-" + tail.mkString("")) ::: list.tail))
                case 'h' => {
                  if(map.contains('cmd)) map('cmd).toString match {
                    case "l" => println("Looks up a yoruba definition"); exit(1)
                    case _ => println("Unknown command"); exit(1)
                  }
                  else println(about + usage); exit(1)
                }
                case _ => println("Unknown option -" + char); exit(1)
              }
              case Nil => parseOptions(map, list.tail)
            }
          }
          case string :: Nil => parseOptions(map ++ Map('word -> string), list.tail)
          case _ => println("Error"); exit(1)
        }        
      }
      
      val options = parseOptions(Map(), arguments)
      
      //println (options)
      //val dict = parse(options.get('path).get.toString)      
  }
}