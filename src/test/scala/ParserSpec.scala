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
	
	/*
	it can "parse assimilated opposing tone combinations" in {
	  var assimilated = parser.parse(parser.wordEntry, assimilatedWord2).get
	  assert(assimilated._1.word.toString == "abamo")
	}
	*/
}
