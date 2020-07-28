package viper.server.vsi

import ch.qos.logback.classic.Logger
import viper.server.core.ViperCache.{_backendSpecificCache, _cache, _node_hash_memo, get, getHashForNode}
import viper.silver.ast.{Method, Node, Program}
import viper.silver.verifier.AbstractVerificationError

import scala.collection.mutable

import scala.collection.mutable.{Map => MutableMap}


trait VerificationServerInterfaceCache {

  /** The cache infrastructure is a nested MutableMap. They can be described as follows:
    * (FileName -> (hashString -> cacheEntries))
    *
    * The inner map is referred to as the fileCache. As the name indicates, it stores,
    * for each file, a number of hashes and corresponding cache entries.
    */
  protected val _cache = MutableMap[String, MutableMap[String, List[CacheEntry]]]()

  override def toString: String = _cache.toString

  def contains(backendName: String, file: String, hash: String): Boolean = {
    get(backendName, file, hash).nonEmpty
  }

  protected def getKey(backendName: String, file: String): String = file

  def get(backendName: String, file: String, hash: String): List[CacheEntry] = {
    assert(hash != null)
    val key = getKey(backendName, file)

    _cache.get(key) match {
      case Some(fileCache) =>
        fileCache.get(hash) match {
          case Some(cache_entry_list) => cache_entry_list
          case None => Nil
        }
      case None => Nil
    }
  }

  //consider making users pass cache entries directly!
  def update(backendName: String, file: String, hash: String, cacheEntry: CacheEntry): List[CacheEntry] = {
    assert(hash != null)
    implicit val key: String = getKey(backendName, file)

    _cache.get(key) match {
      case Some(fileCache) =>
        // If a fileChace exists for given file ...
        try {
          //get exisitng list of entries (or an empty list, if none exist yet) and prepend new entry
          val existing_entries = fileCache.getOrElse(hash, Nil)
          val updated_cacheEntries = cacheEntry :: existing_entries

          //set new hash/cacheEntries pair in map
          fileCache(hash) = updated_cacheEntries
          updated_cacheEntries
        } catch {
          case e: Exception =>
            fileCache(hash)
        }
      case None =>
        //if file not in cache yet, create new map entry for it, restart the function
        _cache += (key -> collection.mutable.Map[String, List[CacheEntry]]())
        update(backendName, file, hash, cacheEntry)
    }
  }



  def forgetFile(backendName: String, file: String): Option[String] = {
    val key = getKey(backendName, file)
    _cache.remove(key) match {
      case Some(_) => Some(key)
      case None => None
    }
  }

  def resetCache(): Unit = {
    _cache.clear()
  }

  protected def hex(h: String) = h.hashCode.toHexString
}


// ===== AUXILIARY CLASSES ==================================================================

/** This trait is a generic wrapper for cache entries.
  *
  * Extending this specifies what the cache will be holding.
  * */
trait CacheEntry {

}