import collection._
import YorubaImplicits._
import java.io.RandomAccessFile
import scala.io.Source

case class YorubaDictionary(val self:Map[WordEntry, List[Meaning]] = Map[WordEntry, List[Meaning]]()) extends MapProxy[WordEntry, List[Meaning]] {
  override def +[B1 >: List[Meaning]](kv: (WordEntry, B1)) : YorubaDictionary = {
    val (key, value) = kv
    if(self.contains(key)) {
      var meanings = self.getOrElse(key, List()) ++: value.asInstanceOf[List[Meaning]]
      YorubaDictionary(self.updated(key, meanings))
    } else {
      YorubaDictionary(self.updated(key, value.asInstanceOf[List[Meaning]]))
    }
  }
  
  def ++(xs: Map[WordEntry, List[Meaning]]): YorubaDictionary = {
    YorubaDictionary(self ++ xs)
  }
}

case class IndexedDictionary(val index:Map[String, Long], val filename:String) extends FileParser {

  def lookup(word:Any):YorubaDictionary = {
    def strip(yoruba:String):String = {
      import java.text.Normalizer
      Normalizer.normalize(yoruba, Normalizer.Form.NFD).replaceAll("\\p{InCombiningDiacriticalMarks}+", "").toLowerCase()
    }

    val entries = index.keys.filter(entry => strip(entry) == strip(word.toString)).toList.distinct
    val result = entries.map { entry =>
      val offset = index.get(entry).get
      val file = new RandomAccessFile(filename, "r")
      file.seek(offset)
      val line = file.readLine()
      file.close()
      parse(wordEntry, new String(line.getBytes("ISO-8859-1"), "UTF-8")).get
    }.toMap
    YorubaDictionary(result)
  }

  def strictLookup(word:Any):YorubaDictionary = {
    val entries = index.keys.filter(entry => entry == word.toString).toList.distinct
     val result = entries.map { entry =>
      val offset = index.get(entry).get
      val file = new RandomAccessFile(filename, "r")
      file.seek(offset)
      val line = file.readLine()
      file.close()
      parse(wordEntry, new String(line.getBytes("ISO-8859-1"), "UTF-8")).get
    }.toMap
    YorubaDictionary(result)
  }

  def lookupRelated(word:Any):YorubaDictionary = {
    val fileSource = Some(Source.fromFile(filename)(CODEC))
    val results = fileSource.get.getLines.filterNot(_.startsWith(COMMENT)).flatMap { line =>
      val parsed = parse(wordEntry, line)
      if (parsed.successful) {
        val (entry, meanings) = parsed.get
        if (entry.word.decomposition.map(_.toYoruba).contains(word.toString) || entry.word.toYoruba == word.toString) {
          Some(entry -> meanings)
        } else None
      } else None
    }.toMap
    fileSource.foreach(_.close())
    YorubaDictionary(results)
  }

  def lookupDerivatives(word:Any):YorubaDictionary = {
    val fileSource = Some(Source.fromFile(filename)(CODEC))
    val results = fileSource.get.getLines.filterNot(_.startsWith(COMMENT)).flatMap { line =>
      val parsed = parse(wordEntry, line)
      if (parsed.successful) {
        val (entry, meanings) = parsed.get
        if (entry.word.root.toYoruba == word.toString) {
          Some(entry -> meanings)
        } else None
      } else None
    }.toMap
    fileSource.foreach(_.close())
    YorubaDictionary(results)
  }
}

object DictionaryImplicits {
  implicit def map2dict(map:Map[WordEntry, List[Meaning]]):YorubaDictionary = YorubaDictionary(map)
}

