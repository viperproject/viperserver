package viper.server.vsi

import viper.silver.utility.CacheHelper

import scala.collection.mutable.{Map => MutableMap}


/** The goal of this generic caching trait is to provide
  *
  * 1) an elaborate storing system that can decide whether or not stored resources are valid.
  *
  * in order to implement
  *
  * 2) A very simple caching API
  *
  * A verification server should - very bluntly expressed - take a program and return a (potentially
  * empty) set of errors. This process is assumed to be deterministic. I.e., verifying the same program twice
  * results in the exact same verification result. Of course, changing the program is likely to change
  * the verification outcome. However, it may well be that the outcome will not completely change. E.g.,
  * if a program consists of two totally independent methods, changing the first may have absolutely
  * no effect on the verification results of the second. In such a situation resources can be economized
  * by reverifying only the first method and not reverifying the second. Sine one of the methods is not reverified
  * its verifications (from a previous verification) must be stored in order to return them.
  *
  * As mentioned earlier, this requires an elaborate storage system that can
  *
  *   1) store verification results for individual program members
  *   2) decide whether or not stored results are still valid if the program is changed.
  *
  * Once such a system is in place it can be used to implement a simple API. This API should take a
  * program P, analyse it, transforms it and return a program P' as well as a set of verification results.
  * This program should thereby have been transformed in such a way that a verifier may not perform
  * unnecessary verifications on its members.
  * */
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
  protected val _cache = MutableMap[String, FileCash]()
  type FileCash = MutableMap[Hash, List[CacheEntry]]


  protected def createCacheEntry(content: CacheContent, dependencyHash: Hash): CacheEntry

  def get(
        file_key: String,
        key: Concerning,
        dependencies: List[Concerning]): Option[CacheEntry] = {

    val concerning_hash = hashFunction(key)
    val dependencies_hash = dependencies.map(hashFunction).mkString(" ")
    val dependency_hash = CacheHelper.buildHash(concerning_hash + dependencies_hash)
    assert(concerning_hash != null)

    for {
      fileCache <- _cache.get(file_key)
      cacheEntries <- fileCache.get(concerning_hash)
      validEntry <- cacheEntries.find(_.depencyHash == dependency_hash)
    } yield validEntry
  }

  def update(
        file_key: String,
        key: Concerning,
        dependencies: List[Concerning],
        content: CacheContent): List[CacheEntry] = {

    val concerning_hash = hashFunction(key)
    val dependencies_hash = dependencies.map(hashFunction).mkString(" ")
    val dependency_hash = CacheHelper.buildHash(concerning_hash + dependencies_hash)
    val cacheEntry: CacheEntry = createCacheEntry(content, dependency_hash)

    assert(concerning_hash != null)

    _cache.get(file_key) match {
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
        _cache += (file_key -> collection.mutable.Map[Hash, List[CacheEntry]]())
        update(file_key, key, dependencies, content)
    }
  }

  def forgetFile(file_key: String): Option[String] = {
    _cache.remove(file_key) match {
      case Some(_) => Some(file_key)
      case None => None
    }
  }

  def resetCache(): Unit = {
    _cache.clear()
  }
}

// ===== AUXILIARY TRAITS ==================================================================

/** This trait is a generic wrapper for cache entries.
  *
  * Extending this specifies what the cache will be holding.
  * */
abstract class CacheEntry (val cacheContent: CacheContent, val depencyHash: String)


trait CacheContent {

}
