import collection._
import YorubaImplicits._
import DictionaryImplicits._


/**
 * 
 */
case class YorubaDictionary(val self:Map[Word, List[Meaning]]) extends MapProxy[Word, List[Meaning]] {
  def lookup(word:Any):YorubaDictionary = self.filterKeys(k => (k.spelling == word) || (k == word)) 
  def lookupRelated(word:Any) = self filterKeys (k => (k.decomposition contains word) || (k == word))
}

/**
 * 
 */
object DictionaryImplicits {
  implicit def map2dict(map:Map[Word, List[Meaning]]):YorubaDictionary = YorubaDictionary(map)
}

