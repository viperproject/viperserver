package viper.server.vsi

import ch.qos.logback.classic.Logger
import scala.collection.mutable.{Map => MutableMap}


trait VerificationServerInterfaceCache {

  override def toString: String = _cache.toString

  /** This abstract types define a generic version of a cache.
    *
    * The [[Preimage]] is the Input type to the hash function, [[Hash]] the output. Typically, a
    * Preimage would be overriden to some notion of an AST while the Hash would likely be a String.
    * */
  type Preimage
  type Hash
  def hashFunction(in: Preimage): Hash

  /** The cache infrastructure is a nested, mutable Map. The maps can be described in terms of their
    * key and values as follows:  (FileName -> (hashString -> cacheEntries)).
    *
    * The inner map is referred to as the fileCache. As the name indicates, it stores,
    * for each file, a number of hashes and corresponding cache entries.
    */
  type FileCash = MutableMap[Hash, List[CacheEntry]]
  protected val _cache = MutableMap[String, FileCash]()

  /** This is a utility function that generates Filenames for specific backends.
    *
    * This can be used when extending VsiCache as a single object while verifying files with
    * various backends. In that case this method need overriding to produce the desired tag.
    * */
  protected def getKey(file: String, backendName: String = ""): String = file

  def contains(backendName: String, file: String, p: Preimage): Boolean = {
    get(backendName, file, p).nonEmpty
  }

  def get(backendName: String, file: String, key: Preimage): List[CacheEntry] = {
    val hash: Hash = hashFunction(key)
    assert(hash != null)
    val file_key = getKey(backendName, file)

    _cache.get(file_key) match {
      case Some(fileCache) =>
        fileCache.get(hash) match {
          case Some(cache_entry_list) => cache_entry_list
          case None => Nil
        }
      case None => Nil
    }
  }

  def update(backendName: String, file: String, key: Preimage, cacheEntry: CacheEntry): List[CacheEntry] = {
    val hash: Hash = hashFunction(key)
    assert(hash != null)
    implicit val fileKey: String = getKey(backendName, file)

    _cache.get(fileKey) match {
      case Some(fileCache) =>
        // If a fileChace exists for given file ...

        try { //TODO find out if try catch ever does something
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
        _cache += (fileKey -> collection.mutable.Map[Hash, List[CacheEntry]]())
        update(backendName, file, key, cacheEntry)
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

  protected def hex(h: Hash) = h.hashCode.toHexString
}


// ===== AUXILIARY TRAITS ==================================================================
/** This trait is a generic wrapper for cache entries.
  *
  * Extending this specifies what the cache will be holding.
  * */
trait CacheEntry {
}