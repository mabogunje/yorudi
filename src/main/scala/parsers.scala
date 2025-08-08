import scala.util.parsing.combinator._
import Bias._
import DictionaryImplicits._
import YorubaImplicits._
import io._
import scala.io.Codec
import java.io.RandomAccessFile

class GrammarParser extends RegexParsers {
  /** The Grammar Parser uses a series of combinatorial parsers
   *  to build a single parser that can be used to parse certain
   *  formatted strings into Dictionary Word Definitions according
   *  to the classes defined in grammar.scala   
   */
  
  // Base token parsers - indicating word properties
  def root:Parser[SpeechProperty] = """\*""".r ^^^ {Root}
  def connector:Parser[SpeechProperty] = """~""".r ^^^ {Connector}
  
  def asmL:Parser[Assimilated] = """<(\+)+""".r ^^ { str => Assimilated(Left, str.count(_ == '+')) }
  def asmR:Parser[Assimilated] = """(\+)+>""".r ^^ { str => Assimilated(Right, str.count(_ == '+')) }
  def assimilation:Parser[Assimilated] = asmL | asmR
  
  def elsL:Parser[Elided] = """<(\-)+""".r ^^ { str => Elided(Left, str.count(_ == '-')) }
  def elsR:Parser[Elided] = """(\-)+>""".r ^^ { str => Elided(Right, str.count(_ == '-')) }
  def elision:Parser[Elided] = elsL | elsR

  def prefixes:Parser[SpeechProperty] = asmL|elsL
  def postfixes:Parser[SpeechProperty] = asmR|elsR|root
  def property:Parser[SpeechProperty] = root|assimilation|elision

  // Base parsers for all values - applies restrictions on acceptable strings
  def term:Parser[String] = """[\p{L}(\p{Mn})?\\~?]+[\p{L}(\p{Mn})?]*""".r ^^ {_.toLowerCase()}
  def value:Parser[String] = """[\w|\(\)|\-|']+""".r ^^ {_.toString()}

  // Base parser for word senses: Strings delimited by '/'. May be whole sentences 
  def sense:Parser[String] = "/" ~ rep(value) ^^ { case "/" ~ list => list mkString " " }

  // Base parser for attributes: User-defined key-value pairings separated by ':'
  def attribute:Parser[(String,String)] = value ~ ":" ~ value ^^ { 
    case k~":"~v => (k -> v)
  }
  
  /**
   * Now we build the compound parsers which will produce our grammar objects
   */
  
  def word:Parser[Yoruba] = prefixes.* ~ term ~ postfixes.* ^^ {
    case plist1~term~plist2 => Term(term, (plist1 union plist2))
  }
  
  def decomposition:Parser[List[Yoruba]] = "[" ~repsep(word, ".")~ "]" ^^ {
    case "[" ~ list ~ "]" => {list}
  }
  
  def glossary:Parser[List[Meaning]] = rep(sense) ^^ { _ map (Translation(_)) }
  
  def attribs:Parser[List[(String,String)]] = "<"~ repsep(attribute, "|") ~">" ^^ {
    case "<" ~ list ~ ">" => { list }    
  }

  def wordEntry:Parser[(WordEntry, List[Meaning])] = term ~ decomposition ~ glossary ~ attribs.? ^^ {
    case term ~ dcomp ~ gloss ~ attrs => {
      var entry = WordEntry(Word(term, dcomp), attrs.getOrElse(List()).toMap)
      ((entry -> gloss))
    }
  }
}

class FileParser extends GrammarParser {
  val CODEC = Codec.UTF8
  val COMMENT = "#";
  val DIRECTIVE = "!";
  var LANGUAGE = "";
  var DICT = YorubaDictionary()
  var problems = List
  
  def parse(filename:String):YorubaDictionary = {
    var fileSource: Option[scala.io.Source] = None
    try {
      fileSource = Some(Source.fromFile(filename)(CODEC))
      val entries = fileSource.get.getLines.filterNot(_.startsWith(COMMENT)) map {
        parse(wordEntry, _)
      } filter {_.successful} map {_.get} toList
      
      DICT ++ entries
    } finally {
      fileSource.foreach(_.close())
    }
  }

  def index(filename:String):Map[String, Long] = {
    val file = new RandomAccessFile(filename, "r")
    var offset = file.getFilePointer()
    var line = file.readLine()
    var result = Map[String, Long]()
    while (line != null) {
      val parsed = parse(wordEntry, new String(line.getBytes("ISO-8859-1"), "UTF-8"))
      if (parsed.successful) {
        val (entry, _) = parsed.get
        result += (entry.word.toYoruba -> offset)
      }
      offset = file.getFilePointer()
      line = file.readLine()
    }
    file.close()
    result
  }
}
