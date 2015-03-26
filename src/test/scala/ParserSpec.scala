import org.scalatest.FlatSpec

class ParserSpec extends FlatSpec {
	val parser:GrammarParser = new GrammarParser()
	val rootWord = "igba [ìgbà*]  /time"
	val linkedWord = "ade [à . dé*]  /crown"
	val elidedWord = "a [awa-->*]  /we"
	val elidedWord2 = "nigba [ní . <-ìgbà*]  /when"
	val assimilatedWord = "kuule [kú+++>* . <++ilé]  /greetings"
	
	"Parser" can "parse simple word definitions" in {
	  var simple = parser.parse(parser.wordEntry, rootWord).get
	  assert(simple._1.word.toString == "ìgbà")
	  assert(simple._1.word.root.toString == "ìgbà")
	  assert(simple._2.head.toString == "time")	  
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
	
	it can "parse elided word combinations" in {
	  var elidedCombo = parser.parse(parser.wordEntry, elidedWord2).get
	  assert(elidedCombo._1.word.toString == "nígbà")
	  assert(elidedCombo._1.word.root.toString == "ìgbà")
	  assert(elidedCombo._2.head.toString == "when")
	}
	
	it can "parse assimilated word combinations" in {
	  var assimilated = parser.parse(parser.wordEntry, assimilatedWord).get
	  assert(assimilated._1.word.toString == "kúulé")
	  assert(assimilated._1.word.root.toString == "kú")
	  assert(assimilated._2.head.toString == "greetings")
	}
	
	
}