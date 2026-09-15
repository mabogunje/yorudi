/**
 * @author damola
 *
 */
package net.mabogunje.yorudi

import org.scalatest.FlatSpec

class ParserSpec extends FlatSpec {
	val parser:GrammarParser = new GrammarParser()
	val rootWord = "igba [ìgbà*]  /time"
	val linkedWord = "ade [à . dé*]  /crown"
	val elidedWord = "a [awa-->*]  /we"
	val elidedWord2 = "abunni [a . bùn . <-ẹni*]  /giver"
	val assimilatedWord = "kuule [kú+++>* . <++ilé]  /greetings"
	val assimilatedWord2 = "abamo [a . <-ìbá+> . <+ọ̀mọ̀*]  /a potter"
	val dottedWord = "ba [bẹ->* . awa-->]  /with"
	val hyphenatedWord = "abaniku~ore [a . ba . <-eni . kù . ~ . ọrẹ*]  /a faithful friend"
	
	"The File Parser" can "parse simple word definitions" in {
	  var simple = parser.parse(parser.wordEntry, rootWord).get
	  assert(simple._1.word.toString == "ìgbà")
	  assert(simple._1.word.root.toString == "ìgbà")
	  assert(simple._2.head.toString == "time")	  
	}
	
	it can "parse underdotted word definitions" in {
	  var underdotted = parser.parse(parser.wordEntry, dottedWord).get
	  assert(underdotted._1.word.toString == "ba")
	}
	
	it can "parse hyphenated word definitions" in {
	  var hyphenated = parser.parse(parser.wordEntry, hyphenatedWord).get
	  assert(hyphenated._1.word.toString == "abanikù~ọrẹ")
	  assert(hyphenated._1.word.root.toString == "ọrẹ")
	  assert(hyphenated._2.head.toString == "a faithful friend")
	}
	
	it can "parse linked word definitions" in {
	  var linked = parser.parse(parser.wordEntry, linkedWord).get
	  assert(linked._1.word.toString == "àdé")
	  assert(linked._1.word.root.toString == "dé")
	  assert(linked._2.head.toString == "crown")
	}
	
	it can "parse elided word definitions" in {
	  var elided = parser.parse(parser.wordEntry, elidedWord).get
	  assert(elided._1.word.toString == "a")
	  assert(elided._1.word.root.toString == "awa")
	  assert(elided._2.head.toString == "we")	  
	}

	it can "parse assimilated word combinations" in {
	  var assimilated = parser.parse(parser.wordEntry, assimilatedWord).get
	  assert(assimilated._1.word.toString == "kúulé")
	  assert(assimilated._1.word.root.toString == "kú")
	  assert(assimilated._2.head.toString == "greetings")
	}
	
	it can "parse elided word combinations" in {
	  var elidedCombo = parser.parse(parser.wordEntry, elidedWord2).get
	  assert(elidedCombo._1.word.toString == "abùnni")
	  assert(elidedCombo._1.word.root.toString == "ẹni")
	  assert(elidedCombo._2.head.toString == "giver")
	}

	it should "ignore comments, directives, and blank lines when parsing dictionary lines" in {
	  val fileParser = new FileParser()
	  val lines = IndexedSeq(
	    "#!author: Damola Mabogunje",
	    "!lang: en",
	    "",
	    "# This is a comment",
	    "ade [à . dé*]  /crown"
	  )

	  val result = fileParser.parseDictionaryLines("test.yor", lines)

	  assert(result.right.get.size == 1)
	  assert(result.right.get.head._1 == 5)
	  assert(result.right.get.head._2._1.word.toString == "àdé")
	}

	it should "report invalid dictionary entries with source line numbers" in {
	  val fileParser = new FileParser()
	  val lines = IndexedSeq(
	    "# Comment",
	    "ade [à . dé*]  /crown",
	    "not a valid dictionary entry"
	  )

	  val result = fileParser.parseDictionaryLines("broken.yor", lines)

	  val error = result.left.get.head
	  assert(error.source == "broken.yor")
	  assert(error.lineNumber == 3)
	  assert(error.line == "not a valid dictionary entry")
	  assert(error.message.contains("["))
	}

	it should "parse entries with inline comments" in {
	  val fileParser = new FileParser()
	  val line = "dirin [dì++> . <+irin*] /limp /stumble  # Dictionary edit marker"

	  val result = fileParser.parseDictionaryLine("test.yor", 1, line)

	  assert(result.right.get._1.word.toString == "dirin")
	  assert(result.right.get._2.map(_.description) == List("limp", "stumble"))
	}

	it should "parse exclamation marks in glossary text" in {
	  val fileParser = new FileParser()
	  val line = "oluwaseun [olúwa . ṣeun*] /Thanks to the Lord!  /The Lord is good"

	  val result = fileParser.parseDictionaryLine("test.yor", 1, line)

	  assert(result.right.get._1.word.toString == "olúwaṣeun")
	  assert(result.right.get._2.map(_.description) == List("Thanks to the Lord!", "The Lord is good"))
	}

	it should "report dictionary file parser errors" in {
	  val fileParser = new FileParser()
	  val error = intercept[IllegalArgumentException] {
	    fileParser.indexFile("dicts/broken.en.yor")
	  }

	  assert(error.getMessage.contains("Dictionary contains invalid entries"))
	  assert(error.getMessage.contains("dicts/broken.en.yor:6"))
	  assert(error.getMessage.contains("ko /not"))
	}
	
	/*
	it can "parse assimilated opposing tone combinations" in {
	  var assimilated = parser.parse(parser.wordEntry, assimilatedWord2).get
	  assert(assimilated._1.word.toString == "abamo")
	}
	*/
}
