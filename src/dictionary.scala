import collection._
import YorubaImplicits._
import DictionaryImplicits._


/**
 * 
 */
case class YorubaDictionary(val self:Map[WordEntry, List[Meaning]]) extends MapProxy[WordEntry, List[Meaning]] {
  def lookup(word:Any):YorubaDictionary = self.filterKeys(_.word == word)
  def lookupRelated(word:Any) = self filterKeys (k => (k.word.decomposition contains word) || (k.word == word))
}

/**
 * 
 */
object DictionaryImplicits {
  implicit def map2dict(map:Map[WordEntry, List[Meaning]]):YorubaDictionary = YorubaDictionary(map)
}

