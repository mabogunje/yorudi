import collection._
import YorubaImplicits._
import org.scalatest.FlatSpec

class DictionarySpec extends FlatSpec {
  var dict = new YorubaDictionary()

  "A Yoruba Dictionary" should "be a map" in {
    assert(dict == Map())
  }
  
  it can "be added to" in {
    val word = Word("dé", List("dé" as Root))
    val meaning = (Translation("crown"))
    val attribs = immutable.HashMap[String, String]()
    
    dict + ( (WordEntry(word, attribs), List(meaning)) )

    assert(dict.size == 1)
  }
}