/**
 * @author damola
 *
 */
package net.mabogunje.yorudi

import YorubaImplicits._
import scala.collection.immutable
import org.scalatest.flatspec.AnyFlatSpec

class DictionarySpec extends AnyFlatSpec {
  val emptyDict = YorubaDictionary()

  "A Yoruba Dictionary" should "start empty" in {
    assert(emptyDict.size == 0)
  }
  
  it can "be added to" in {
    val word = Word("dé", List("dé" as Root))
    val meaning = (Translation("put atop"))
    val attribs = immutable.HashMap[String, String]()
    val entry = (WordEntry(word, attribs), List(meaning))
    
    val newDict = emptyDict + entry
    assert(newDict.size == 1)
  }
  
  it can "be removed from" in {
    val word = Word("dé", List("dé" as Root))
    val meaning = (Translation("put atop"))
    val attribs = immutable.HashMap[String, String]()
    val entry = (WordEntry(word, attribs), List(meaning))
    
    val dict = YorubaDictionary(Map(entry))
    assert(dict.size == 1)
    
    val newDict = dict - entry._1
    assert(newDict.size == 0)
  } 
  
  it should "merge duplicate words with different meanings" in {
    val word = Word("dé", List("dé" as Root))
    val meaningA = (Translation("put atop"))
    val meaningB = (Translation("place somewhere"))
    val attribs = immutable.HashMap[String, String]()
    val entryA: (WordEntry, List[Meaning]) = (WordEntry(word, attribs), List(meaningA))
    val entryB: (WordEntry, List[Meaning]) = (WordEntry(word, attribs), List(meaningB))
    
    val dict = YorubaDictionary(Map(entryA))
    val newDict = dict + entryB
    
    assert(newDict.size == 1)
    assert(newDict.get(entryA._1).get == List(meaningA, meaningB))
  }
}

class IndexedDictionarySpec extends AnyFlatSpec {
  val testFile = "dicts/sample.en.yor"
  val parser = new FileParser()
  val dict = parser.loadDictionary(testFile)

  "An IndexedDictionary" should "lookup words by tone-insensitive matching" in {
    val result = dict.lookup("ade")
    assert(result.size == 1)
    assert(result.keys.head.word.toYoruba == "àdé")
  }

  it should "lookup words by strict tone-sensitive matching" in {
    val result = dict.strictLookup("àdé")
    assert(result.size == 1)
    assert(result.keys.head.word.toYoruba == "àdé")

    val failedResult = dict.strictLookup("ade")
    assert(failedResult.size == 0)
  }

  it should "lookup related words by decomposition" in {
    val result = dict.lookupRelated("dé")
    assert(result.size == 1)
    assert(result.keys.head.word.toYoruba == "àdé")
  }

  it should "lookup derivatives by root" in {
    val result = dict.lookupDerivatives("dé")
    assert(result.size == 1)
    assert(result.keys.head.word.toYoruba == "àdé")
  }

  it should "merge duplicate entries loaded from parsed dictionary lines" in {
    val result = dict.lookup("ba")

    assert(result.size == 1)
    assert(result.values.head.map(_.description) == List(
      "to meet",
      "overtake",
      "find at a place",
      "with",
      "against",
      "should",
      "would",
      "might",
      "ought"
    ))
  }

  it should "lookup entries constructed from parsed definitions" in {
    val word = Word("ade", List("à", "dé" as Root))
    val dictionary = IndexedDictionary(IndexedSeq(WordEntry(word, immutable.Map[String, String]()) -> List(Translation("crown"))))

    val result = dictionary.lookup("ade")

    assert(result.size == 1)
    assert(result.keys.head.word.toYoruba == "àdé")
  }
}
