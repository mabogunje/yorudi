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
    val meaning = (Translation("put atop"))
    val attribs = immutable.HashMap[String, String]()
    var entry = (WordEntry(word, attribs), List(meaning))
    
    var newDict = dict + entry
    assert(newDict.size == 1)
  }
  
  it can "be removed from" in {
    val word = Word("dé", List("dé" as Root))
    val meaning = (Translation("put atop"))
    val attribs = immutable.HashMap[String, String]()
    var entry = (WordEntry(word, attribs), List(meaning))
    
    dict = YorubaDictionary(Map(entry))
    assert(dict.size == 1)
    
    var newDict = dict - entry._1
    assert(newDict.size == 0)
  } 
  
  it should "merge duplicate words with different meanings" in {
    val word = Word("dé", List("dé" as Root))
    val meaningA = (Translation("put atop"))
    val meaningB = (Translation("place somewhere"))
    val attribs = immutable.HashMap[String, String]()
    var entryA = (WordEntry(word, attribs), List(meaningA.asInstanceOf[Meaning]))
    var entryB = (WordEntry(word, attribs), List(meaningB.asInstanceOf[Meaning]))
    
    dict = new YorubaDictionary(Map(entryA))
    var newDict = dict + entryB
    
    assert(newDict.size == 1)
    assert(newDict.get(entryA._1).get == List(meaningA, meaningB))
  }
}