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
  def root:Parser[List[WordProperty]] = """\*""".r ^^^ {List(Root)}
  
  def asmL:Parser[List[Assimilated]] = """<\+""".r ^^^ { List(Assimilated(Left)) }
  def asmR:Parser[List[Assimilated]] = """\+>""".r ^^^ { List(Assimilated(Right)) }
  def assimilation:Parser[List[Assimilated]] = asmL | asmR
  
  def elsL:Parser[List[Elided]] = """<\-+""".r ^^ { str => 
    for (c <- str.tail.toList) yield Elided(Left) 
  }
  def elsR:Parser[List[Elided]] = """\-+>""".r ^^ {str => 
    for (c <- str.dropRight(1).toList) yield Elided(Right) 
  }
  def elision:Parser[List[Elided]] = elsL | elsR

  def prefixes:Parser[List[WordProperty]] = asmL|elsL
  def postfixes:Parser[List[WordProperty]] = asmR|elsR|root  
  def property:Parser[List[WordProperty]] = root|assimilation|elision 

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
  
  def word:Parser[Expression] = prefixes.* ~ term ~ postfixes.* ^^ {
    case plist1~term~plist2 => Term(term, (plist1.flatten union plist2.flatten):_*)
  }
  
  def decomposition:Parser[List[Expression]] = "[" ~repsep(word, ".")~ "]" ^^ {
    case "[" ~ list ~ "]" => {list}
  }
  
  def glossary:Parser[List[Meaning]] = rep(sense) ^^ { _ map (Translation(_)) }
  
  def attribs:Parser[List[Tuple2[String,String]]] = "<"~ repsep(attribute, "|") ~">" ^^ {
    case "<" ~ list ~ ">" => { list }    
  }

  def wordEntry:Parser[(WordEntry, List[Meaning])] = term ~ decomposition ~ glossary ~ attribs.? ^^ {
    case term ~ dcomp ~ gloss ~ attrs => {
      var entry = WordEntry(Word(term, dcomp))
      for (a <- attrs) { a map (entry.addAttributes(_)) }
      ((entry -> gloss))
    }
  }
}

object ParserTest extends GrammarParser {
  def main(args:Array[String]) {
    var dict = YorubaDictionary(Map[WordEntry, List[Meaning]]())
    val testEntry1 = "igba [ìgbà*]  /time"
    val testEntry2 = "nigba [ní . <-ìgbà*]  /when"
    val testEntry3 = "kuule [kú+>* . <-ilé]  /greetings"
    val testEntry4 = "ade [à . dé*]  /crown"
    val testEntry5 = "a [awa-->*]  /we"
    
    dict += parseAll(wordEntry, testEntry1).get
    dict += parseAll(wordEntry, testEntry2).get
    dict += parseAll(wordEntry, testEntry3).get
    dict += parseAll(wordEntry, testEntry4).get
    dict += parseAll(wordEntry, testEntry5).get
    
     for (entry <- dict) println(entry._1.word, entry._2)
  }
}
