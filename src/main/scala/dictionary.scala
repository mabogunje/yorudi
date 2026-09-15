/**
 * @author damola
 *
 */
package net.mabogunje.yorudi

import collection._
import YorubaImplicits._
import java.text.Normalizer

case class YorubaDictionary(val self:Map[WordEntry, List[Meaning]] = Map[WordEntry, List[Meaning]]()) extends MapProxy[WordEntry, List[Meaning]] {
  override def +[B1 >: List[Meaning]](kv: (WordEntry, B1)) : YorubaDictionary = {
    val (key, value) = kv
    if(self.contains(key)) {
      var meanings = (self.getOrElse(key, List()) ++: value.asInstanceOf[List[Meaning]]).distinct
      YorubaDictionary(self.updated(key, meanings))
    } else {
      YorubaDictionary(self.updated(key, value.asInstanceOf[List[Meaning]]))
    }
  }
  
  def ++(xs: Map[WordEntry, List[Meaning]]): YorubaDictionary = {
    YorubaDictionary(self ++ xs)
  }
}

case class IndexedDictionary(entries:IndexedSeq[(WordEntry, List[Meaning])]) {
  // Helper functions for string comparisons
  def strip(yoruba:String):String = {
    Normalizer.normalize(yoruba, Normalizer.Form.NFD).replaceAll("\\p{InCombiningDiacriticalMarks}+", "").toLowerCase()
  }

  def standardizeDiacritics(yoruba:String):String = {
    Normalizer.normalize(yoruba, Normalizer.Form.NFC).toLowerCase()
  }

  private val exactIndex:Map[String, IndexedSeq[Int]] = indexByOne(entry => standardizeDiacritics(entry.word.toYoruba))
  private val looseIndex:Map[String, IndexedSeq[Int]] = indexByOne(entry => strip(entry.word.toYoruba))
  private val rootIndex:Map[String, IndexedSeq[Int]] = indexByOne(entry => standardizeDiacritics(entry.word.root.toYoruba))
  private val relatedIndex:Map[String, IndexedSeq[Int]] = indexByMany { entry =>
    (entry.word.decomposition.map(term => standardizeDiacritics(term.toYoruba)) :+
      standardizeDiacritics(entry.word.toYoruba)).distinct
  }

  private def indexByOne(key:WordEntry => String):Map[String, IndexedSeq[Int]] = {
    indexByMany(entry => Seq(key(entry)))
  }

  private def indexByMany(keys:WordEntry => Seq[String]):Map[String, IndexedSeq[Int]] = {
    entries.zipWithIndex.flatMap {
      case ((entry, _), idx) => keys(entry).map(_ -> idx)
    }.groupBy(_._1).map {
      case (key, values) => key -> values.map(_._2).toIndexedSeq.distinct
    }
  }

  private def definitionsAt(indexes:IndexedSeq[Int]):YorubaDictionary = {
    indexes.foldLeft(YorubaDictionary()) {
      case (dictionary, idx) => dictionary + entries(idx)
    }
  }

  //Lookup functions
  def lookup(word:Any):YorubaDictionary = {
    definitionsAt(looseIndex.getOrElse(strip(word.toString), IndexedSeq.empty))
  }

  def strictLookup(word:Any):YorubaDictionary = {
    definitionsAt(exactIndex.getOrElse(standardizeDiacritics(word.toString), IndexedSeq.empty))
  }


  def lookupRelated(word:Any):YorubaDictionary = {
    definitionsAt(relatedIndex.getOrElse(standardizeDiacritics(word.toString), IndexedSeq.empty))
  }

  def lookupDerivatives(word:Any):YorubaDictionary = {
    definitionsAt(rootIndex.getOrElse(standardizeDiacritics(word.toString), IndexedSeq.empty))
  }
}

object IndexedDictionary extends FileParser {
  def empty:IndexedDictionary = IndexedDictionary(IndexedSeq.empty)

  def apply(index:Map[String, Int], lines:IndexedSeq[String]):IndexedDictionary = {
    val entries = index.values.toIndexedSeq.distinct.sorted.flatMap { lineIdx =>
      if(lineIdx >= 0 && lineIdx < lines.length) {
        parseDictionaryLine("dictionary", lineIdx + 1, lines(lineIdx)).right.toOption
      } else {
        None
      }
    }
    IndexedDictionary(entries)
  }
}

object DictionaryImplicits {
  implicit def map2dict(map:Map[WordEntry, List[Meaning]]):YorubaDictionary = YorubaDictionary(map)
}
