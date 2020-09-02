package viper.server.vsi

import viper.silver.utility.CacheHelper

import scala.collection.mutable.{ListBuffer, Map => MutableMap}

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
abstract class VerificationServerInterfaceCache {

  override def toString: String = _cache.toString

  /** The cache infrastructure is a nested, mutable Map. The maps can be described in terms of their
    * key and values as follows:  (FileName -> (hashString -> cacheEntries)).
    *
    * The inner map is referred to as the fileCache. As the name indicates, it stores,
    * for each file, a number of hashes and corresponding cache entries.
    */
  protected val _cache = MutableMap[String, FileCash]()
  type FileCash = MutableMap[String, List[CacheEntry]]

  /** This method transforms a program by applying cache's current state.
    *
    * The input and out put program should be equivalent with respect to verification, but they might
    * differ in their AST. In particular the, output program should might be transformed such that
    * verifying it is faster than verifying the input program.
    *
    * Additionally, the method returns previously cached verification result for members of the
    * ast that hit the cache.
    * */
  def apply(
        file_key: String,
        input_prog: AST): (AST, List[CacheEntry]) = {

    val cache_entries: ListBuffer[CacheEntry] = ListBuffer()
    val concerningsToCache: ListBuffer[Concerning] = ListBuffer()
    val concerningsToVerify: ListBuffer[Concerning] = ListBuffer()

    //read errors from cache
    val cachable_members = input_prog.decompose()
    cachable_members.foreach((c: Concerning) => {
      val dependencies = c.getDependencies(input_prog)
      get(file_key, c, dependencies) match {
        case Some(matched_entry) =>
          concerningsToCache += c.transform
          cache_entries += matched_entry
        case None =>
          //Nothing in cache, request verification
          concerningsToVerify += c
      }
    })
    val all_concernings: List[Concerning] = concerningsToCache.toList ++ concerningsToVerify.toList
    val output_prog: AST = input_prog.compose(all_concernings)
    (output_prog, cache_entries.toList)
  }

  /** Utility function to retrieve entries for single members.
    * */
  final def get(
        file_key: String,
        key: Concerning,
        dependencies: List[Member]): Option[CacheEntry] = {

    val concerning_hash = key.hashFunction
    val dependencies_hash = dependencies.map(_.hashFunction).mkString(" ")
    val dependency_hash = CacheHelper.buildHash(concerning_hash + dependencies_hash)
    assert(concerning_hash != null)

    for {
      fileCache <- _cache.get(file_key)
      cacheEntries <- fileCache.get(concerning_hash)
      validEntry <- cacheEntries.find(_.depencyHash == dependency_hash)
    } yield validEntry


  }

  /** This function updates the cache's state by adding/updating entries.
    * */
  def update(
        file_key: String,
        key: Concerning,
        dependencies: List[Member],
        content: CacheContent): List[CacheEntry] = {

    val concerning_hash = key.hashFunction
    val dependencies_hash = dependencies.map(_.hashFunction).mkString(" ")
    val dependency_hash = CacheHelper.buildHash(concerning_hash + dependencies_hash)
    val cacheEntry: CacheEntry = new CacheEntry(key, content, dependency_hash)

    assert(concerning_hash != null)

    _cache.get(file_key) match {
      case Some(fileCache) =>
        val existing_entries = fileCache.getOrElse(concerning_hash, Nil)
        val updated_cacheEntries = cacheEntry :: existing_entries

        fileCache(concerning_hash) = updated_cacheEntries
        updated_cacheEntries
      case None =>
        //if file not in cache yet, create new map entry for it. Recall the function.
        _cache += (file_key -> collection.mutable.Map[String, List[CacheEntry]]())
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
  * Extending this specifies
  *
  *  - What the cache will be holding.
  *  - Which of the AST member it will be holding it for
  *  - The dependency hash of when the entry is stored.
  *
  *  The dependency hash reflects the state of the dependencies at the time when the entry was created.
  *  If at some point a members dependency hash is no longer equal to the one stored here, the
  *  entry is no longer valid.
  * */
class CacheEntry(val concerning: Concerning, val cacheContent: CacheContent, val depencyHash: String) {
}


trait CacheContent {
}

/** This trait is a generic wrapper for an AST.
  *
  * An AST might a number of members for which it might be sensible to store results, errors, etc.
  * In order for the cache to know which these are, an AST must implement the de-/compose methods.
  *
  * These should decompose and AST into a list of all the members for which the cache is to store
  * results and compose an AST given such list of members.
  * */
trait AST {
  def compose(cs: List[Concerning]): AST

  def decompose(): List[Concerning]
}

trait Member {
  def hashFunction(): String
}

/** This trait is a generic wrapper for AST Members for which things are stored in the cache.
  *
  * An AST might a number of members for which it might be sensible to store results, errors, etc.
  * However, there is a set of requirements of any member for which cache is used. This comes in
  * the form of the following three methods
  *
  * hashFunction: A member must be hashable to determined its mapping in the cache.
  *
  * transform: A member that has been retrieved from the cache should not be reverified. Therefore,
  * a member must have an alternative representation that allows the verifier to distinguish if from
  * members that missed the cache and need reverification
  *
  * getDependencies: A member may hit the cache, but the attached verification results might be invalid.
  * Reason for this is that other members in the program have changed in such a way that influences the
  * current member. These influencing members are called dependencies and need to be checked when
  * retrieving a member's attached result from cache.
  * */
trait Concerning extends Member {
  def transform: Concerning

  def getDependencies(ast: AST): List[Member]
}