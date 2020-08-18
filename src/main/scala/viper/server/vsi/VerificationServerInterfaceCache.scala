package viper.server.vsi

import viper.silver.utility.CacheHelper

import scala.collection.mutable.{Map => MutableMap}


trait VerificationServerInterfaceCache {

  override def toString: String = _cache.toString

  /** [[Concerning]] is the Input type to the hash function, [[Hash]] the output. Both the input
    * and the function are client-specific and therefore need to implemented.
    *
    * E.g., in the case of ViperServer, [[Concerning]] is implemented as a Silver [[viper.silver.ast.Method]]. Silver
    * provides an entityHash for methods, that is used to implement the hashFunction
    * */
  type Concerning
  type Hash = String
  protected def hashFunction(in: Concerning): Hash

  /** The cache infrastructure is a nested, mutable Map. The maps can be described in terms of their
    * key and values as follows:  (FileName -> (hashString -> cacheEntries)).
    *
    * The inner map is referred to as the fileCache. As the name indicates, it stores,
    * for each file, a number of hashes and corresponding cache entries.
    */
  type FileCash = MutableMap[Hash, List[CacheEntry]]
  protected val _cache = MutableMap[String, FileCash]()


  /** This method creates a (client-specific) cache entry.
    *
    * [[Content]] is yet again a client-specific Type. Therefore, both need a specific implementation
    *
    * E.g., the cache entries for ViperServer hold LocalizedErrors.
    */
  type Content
  protected def createCacheEntry(content: Content, dependencyHash: Hash): CacheEntry

  /** This is a utility function that generates Filenames for specific backends.
    *
    * This can be used when extending VsiCache as a single object while verifying files with
    * various backends. In that case this method need overriding to produce the desired tag.
    * */
  protected def getKey(file: String, backendName: String = ""): String = file

  def get(backendName: String, file: String, key: Concerning, dependencies: List[Concerning]): Option[CacheEntry] = {
    val concerning_hash = hashFunction(key)
    val dependencies_hash = dependencies.map(hashFunction).mkString(" ")
    val dependency_hash = CacheHelper.buildHash(concerning_hash + dependencies_hash)
    assert(concerning_hash != null)
    val file_key = getKey(backendName, file)

    _cache.get(file_key) match {
      case Some(fileCache) =>
        fileCache.get(concerning_hash) match {
          case Some(cacheEntries) =>
            cacheEntries.find(_.depencyHash == dependency_hash)
          case None => None
        }
      case None => None
    }
  }

  def update(backendName: String, file: String, key: Concerning, dependencies: List[Concerning], content: Content): List[CacheEntry] = {
    val concerning_hash = hashFunction(key)
    val dependencies_hash = dependencies.map(hashFunction).mkString(" ")
    val dependency_hash = CacheHelper.buildHash(concerning_hash + dependencies_hash)
    val cacheEntry: CacheEntry = createCacheEntry(content, dependency_hash)

    assert(concerning_hash != null)
    implicit val fileKey: String = getKey(backendName, file)

    _cache.get(fileKey) match {
      case Some(fileCache) =>
        // If a fileCache exists for given file get existing list of entries
        // (or an empty list, if none exist yet) and prepend new entry
        val existing_entries = fileCache.getOrElse(concerning_hash, Nil)
        val updated_cacheEntries = cacheEntry :: existing_entries

        //set new hash/cacheEntries pair in map
        fileCache(concerning_hash) = updated_cacheEntries
        updated_cacheEntries
      case None =>
        //if file not in cache yet, create new map entry for it, restart the function
        _cache += (fileKey -> collection.mutable.Map[Hash, List[CacheEntry]]())
        update(backendName, file, key, dependencies, content)
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
abstract class CacheEntry (val cacheContent: CacheContent, val depencyHash: String)


trait CacheContent {

}
