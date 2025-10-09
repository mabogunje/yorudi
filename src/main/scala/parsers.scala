/**
 * @author damola
 *
 */
package net.mabogunje.yorudi

import scala.util.parsing.combinator._
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

  @deprecated("This method is not safe for files in JARs. Use indexFile instead.", "0.1")
  def index(filename: String): Map[String, Long] = {
    val file = getClass.getClassLoader.getResourceAsStream(filename)
    val lines = scala.io.Source.fromInputStream(file)(CODEC).getLines()
    var result = Map[String, Long]()
    var offset = 0L

    for (line <- lines) {
      val parsed = parse(wordEntry, line)

      if (parsed.successful) {
        val (entry, _) = parsed.get
        result += (entry.word.toYoruba -> offset)
      }
      offset += line.getBytes(CODEC.charSet).length + 1
    }
    file.close()
    result
  }

  def indexFile(filename: String): (Map[String, Int], IndexedSeq[String]) = {
    val fileStream = getClass.getClassLoader.getResourceAsStream(filename)
    try {
      val lines = scala.io.Source.fromInputStream(fileStream)(CODEC).getLines().toIndexedSeq
      var indexMap = Map[String, Int]()

      for ((line, idx) <- lines.zipWithIndex) {
        val parsed = parse(wordEntry, line)
        if (parsed.successful) {
          val (entry, _) = parsed.get
          indexMap += (entry.word.toYoruba -> idx)
        }
      }
      (indexMap, lines)
    } finally {
      if (fileStream != null) fileStream.close()
    }
  }
}


object Test extends FileParser {
  def main(args:Array[String]) = {
    val (idx, lines) = indexFile("dicts/gpt.en.yor")
    val dict = IndexedDictionary(idx, lines)
    println(dict.lookup("ìwé"))
  }
}
