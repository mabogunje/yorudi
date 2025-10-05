/**
 * @author damola
 *
 */
package net.mabogunje.yorudi

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

class IndexedDictionarySpec extends FlatSpec {
  val testFile = "dicts/sample.en.yor"
  val parser = new FileParser()
  val dict = IndexedDictionary(parser.index(testFile), testFile)

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
}
