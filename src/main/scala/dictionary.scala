import collection._
import YorubaImplicits._
import DictionaryImplicits._


/**
 * 
 */
case class YorubaDictionary(val self:Map[WordEntry, List[Meaning]] = Map[WordEntry, List[Meaning]]()) extends MapProxy[WordEntry, List[Meaning]] {

  // Auxiliary indices for faster lookups
  private val spellingIndex: Map[String, List[WordEntry]] = 
    self.keys.groupBy(_.word.spelling).mapValues(_.toList)

  private val toYorubaIndex: Map[String, List[WordEntry]] = 
    self.keys.groupBy(_.word.toYoruba).mapValues(_.toList)

  private val rootIndex: Map[String, List[WordEntry]] = 
    self.keys.groupBy(_.word.root.toYoruba).mapValues(_.toList)

  private val decompositionIndex: Map[String, List[WordEntry]] = 
    self.keys.flatMap(entry => entry.word.decomposition.map(_.toYoruba).map(decomp => (decomp, entry)))
      .groupBy(_._1).mapValues(_.map(_._2).toList)
     
  override def +[B1 >: List[Meaning]](kv: (WordEntry, B1)) : YorubaDictionary = {
    if(self.contains(kv._1)) {
      var meanings = self.getOrElse(kv._1, List()) ++: kv._2.asInstanceOf[List[Meaning]]
      self.updated(kv._1, meanings)
    } else {
      self.updated(kv._1, kv._2.asInstanceOf[List[Meaning]])
    }
  }
  
  def ++[B1 >: List[Meaning]](xs: GenTraversableOnce[(WordEntry, List[Meaning])]): YorubaDictionary = {
    var merged = for ((k,v) <- xs.toList.groupBy(_._1)) yield (k, v.distinct.flatMap(_._2))
    self ++ merged
  }
  
  def strictLookup(word:Any):YorubaDictionary = {
    val entries = (toYorubaIndex.getOrElse(word.toString, List()) ++ spellingIndex.getOrElse(word.toString, List())).distinct
    YorubaDictionary(entries.flatMap(entry => self.get(entry).map(meanings => (entry, meanings))).toMap)
  }

  def lookup(word:Any):YorubaDictionary = {
    val entries = (spellingIndex.getOrElse(word.toString, List()) ++ toYorubaIndex.getOrElse(word.toString, List())).distinct
    YorubaDictionary(entries.flatMap(entry => self.get(entry).map(meanings => (entry, meanings))).toMap)
  }

  def lookupRelated(word:Any):YorubaDictionary = {
    val entries = (decompositionIndex.getOrElse(word.toString, List()) ++ toYorubaIndex.getOrElse(word.toString, List())).distinct
    YorubaDictionary(entries.flatMap(entry => self.get(entry).map(meanings => (entry, meanings))).toMap)
  }

  def lookupDerivatives(word:Any):YorubaDictionary = {
    val entries = rootIndex.getOrElse(word.toString, List())
    YorubaDictionary(entries.flatMap(entry => self.get(entry).map(meanings => (entry, meanings))).toMap)
  }
}

/**
 * 
 */
object DictionaryImplicits {
  implicit def map2dict(map:Map[WordEntry, List[Meaning]]):YorubaDictionary = YorubaDictionary(map)
}

