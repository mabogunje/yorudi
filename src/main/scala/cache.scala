/**
 * @author damola
 *
 */
package net.mabogunje.yorudi

import scala.collection.mutable

object DictionaryCache {
    private val cache = mutable.Map[String, IndexedDictionary]()

    def getDictionary(name: String, path: String): IndexedDictionary = {
        cache.getOrElseUpdate(name, {
            val parser = Yorudi
            println(s"Loading dictionary '$name' from '$path' into cache.")
            parser.loadDictionary(path)
        })
    }
}
