/**
 * @author damola
 *
 */
package net.mabogunje.yorudi

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

  def parseOptions(map:OptionMap, list:List[String]):Either[String, OptionMap] = {
    def isSwitch(s:String) = s.startsWith("-")

    def requireValue(option:String, tail:List[String]):Either[String, (String, List[String])] = {
      tail match {
        case value :: rest if !isSwitch(value) => Right((value, rest))
        case _ => Left("Missing value for " + option)
      }
    }

    def parseEqualOption(option:String, arg:String):Either[String, String] = {
      val prefix = option + "="
      if(arg.length > prefix.length) Right(arg.substring(prefix.length))
      else Left("Missing value for " + option)
    }

    def validate(options:OptionMap):Either[String, OptionMap] = {
      val dictKey = options.get('dict).map(_.toString)
      val format = options.get('format).map(_.toString)

      if(dictKey.isEmpty) Left("Missing required option: --dict")
      else if(!dictionaries.contains(dictKey.get)) Left("Unknown dictionary: " + dictKey.get)
      else if(options.get('word).isEmpty) Left("Missing word to look up")
      else if(format.exists(!printers.contains(_))) Left("Unknown format: " + format.get)
      else Right(options)
    }

    def parse(map:OptionMap, list:List[String]):Either[String, OptionMap] = list match {
      case Nil => validate(map)
      case "--dict" :: tail =>
        requireValue("--dict", tail) match {
          case Right((value, rest)) => parse(map ++ Map('dict -> value), rest)
          case Left(error) => Left(error)
        }
      case arg :: tail if arg.startsWith("--dict=") =>
        parseEqualOption("--dict", arg) match {
          case Right(value) => parse(map ++ Map('dict -> value), tail)
          case Left(error) => Left(error)
        }
      case "--fmt" :: tail =>
        requireValue("--fmt", tail) match {
          case Right((value, rest)) => parse(map ++ Map('format -> value), rest)
          case Left(error) => Left(error)
        }
      case arg :: tail if arg.startsWith("--fmt=") =>
        parseEqualOption("--fmt", arg) match {
          case Right(value) => parse(map ++ Map('format -> value), tail)
          case Left(error) => Left(error)
        }
      case "-s" :: tail => parse(map ++ Map('mode -> "strict"), tail)
      case "-g" :: tail => parse(map ++ Map('mode -> "glossary"), tail)
      case "-d" :: tail => parse(map ++ Map('mode -> "derivative"), tail)
      case option :: _ if isSwitch(option) => Left("Invalid option: " + option)
      case word :: tail if map.contains('word) => Left("Unexpected argument: " + word)
      case word :: tail => parse(map ++ Map('word -> word), tail)
    }

    parse(map, list)
  }

  def main(args: Array[String]) {
      if (args.isEmpty) {
        println(usage)
        sys.exit
      }
      val arguments = args.toList
            
      val options = parseOptions(Map(), arguments) match {
        case Right(parsedOptions) => parsedOptions
        case Left(error) => {
          println(error)
          println(usage)
          sys.exit
        }
      }

      val dictKey = options.get('dict).get.toString

      val dictFile = dictionaries(dictKey)
      val (index, lines) = indexFile(dictFile)
      val dict = IndexedDictionary(index, lines)
      val word = options.get('word).getOrElse("")
      var mode = options.get('mode).getOrElse("dictionary")
      var outputType = options.get('format).getOrElse("plain")
      var results = YorubaDictionary()
      var printer:YorudiWriter = printers(outputType.toString)
      	
      mode match {
        case "glossary" => results = dict.lookupRelated(word)
        case "derivative" => results = dict.lookupDerivatives(word)
        case "strict" => results = dict.strictLookup(word)
        case _ => results = dict.lookup(word)
      }
      
      println(printer.writeGlossary(results))
  }
}
