import scala.util.parsing.combinator._
import Bias._
import DictionaryImplicits._
import YorubaImplicits._

class GrammarParser extends RegexParsers {
  /** The Grammar Parser uses a series of combinatorial parsers
   *  to build a single parser that can be used to parse certain
   *  formatted strings into Dictionary Word Definitions according
   *  to the classes defined in grammar.scala   
   */
  
  // Base token parsers - indicating word properties
  def root:Parser[WordProperty] = """\*""".r ^^^ {Root}
  
  def asmL:Parser[Assimilated] = """<\+""".r ^^^ {Assimilated(Left)}
  def asmR:Parser[Assimilated] = """\+>""".r ^^^ {Assimilated(Right)}
  def assimilation:Parser[Assimilated] = asmL | asmR
  
  def elsL:Parser[Elided] = """<\-""".r ^^^ {Elided(Left)}
  def elsR:Parser[Elided] = """\->""".r ^^^ {Elided(Right)}
  def elision:Parser[Elided] = elsL | elsR

  def property:Parser[WordProperty] = root|assimilation|elision 

  // Base parsers for all values - applies restrictions on acceptable strings
  def term:Parser[String] = """\p{L}+""".r ^^ {_.toLowerCase()}
  def value:Parser[String] = """[\w|\(\)]+""".r ^^ {_.toString()}

  // Base parser for word senses: Strings delimited by '/'. May be whole sentences 
  def sense:Parser[String] = "/" ~ rep(value) ^^ { case "/" ~ list => list mkString " " }

  // Base parser for attributes: User-defined key-value pairings separated by ':'
  def attribute:Parser[Tuple2[String,String]] = value ~ ":" ~ value ^^ { 
    case k~":"~v => (k -> v)
  }
  
  /**
   * Now we build the compound parsers which will produce our grammar objects
   */
  
  def word:Parser[Expression] = property.* ~ term ~ property.* ^^ {
    case plist1~term~plist2 => Term(term, (plist1 union plist2):_*)
  }
  
  def decomposition:Parser[List[Expression]] = "[" ~repsep(word, ".")~ "]" ^^ {
    case "[" ~ list ~ "]" => {list}
  }
  
  def glossary:Parser[List[Meaning]] = rep(sense) ^^ { _ map (Translation(_)) }
  
  /*def attribs:Parser[List[Tuple2[String,String]]] = "<"~ repsep(attribute, "|") ~">" ^^ {
    case "<" ~ list ~ ">" => { list }    
  }*/

  def wordEntry:Parser[(Word, List[Meaning])] = term ~ decomposition ~ glossary ^^ {
    case term ~ dcomp ~ gloss => {
      var entry = Word(term, dcomp)
      ((entry -> gloss))
    }
  }
}

object ParserTest extends GrammarParser {
  def main(args:Array[String]) {
    var dict = YorubaDictionary(Map[Word, List[Meaning]]())
    val testEntry1 = "igba [ìgbà*]  /time"
    val testEntry2 = "nigba [ní . <-ìgbà*]  /when"
    val testEntry3 = "kuule [kú+>* . <-ilé]  /greetings"
    val testEntry4 = "ade [à . dé*]  /crown"
    val testEntry5 = "a [awa->->*]  /we"
    
    dict += parseAll(wordEntry, testEntry1).get
    dict += parseAll(wordEntry, testEntry2).get
    dict += parseAll(wordEntry, testEntry3).get
    dict += parseAll(wordEntry, testEntry4).get
    dict += parseAll(wordEntry, testEntry5).get
    
    println(dict.lookupRelated("ìgbà"))
    println(dict.lookup("a"))
  }
}
