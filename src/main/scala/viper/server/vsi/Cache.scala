// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2020 ETH Zurich.

package viper.server.vsi

import viper.silver.utility.CacheHelper

import scala.collection.mutable
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
  * no effect on the verification results of the second. In such a situation, resources can be economized
  * by storing the results, reverifying only the first method and not reverifying the second. This requires
  * a storage system that can
  *
  *   1) store verification results for individual program members
  *   2) decide whether or not stored results are still valid if the program is changed.
  *
  * Once such a system is in place it can be used to implement a simple API. This API should take a
  * program P, analyse it, transforms it and return a program P' as well as a set of verification results.
  * This program should thereby have been transformed in such a way that a verifier may not perform
  * unnecessary verifications on its members.
  * */
abstract class Cache {

  override def toString: String = _cache.toString

  /** The cache infrastructure is a nested, mutable Map. The maps can be described in terms of their
    * key and values as follows:  (FileName -> (hashString -> cacheEntries)).
    *
    * The inner map is referred to as the fileCache. As the name indicates, it stores,
    * for each file, a number of hashes and corresponding cache entries.
    */
  protected val _cache = MutableMap[String, FileCash]()
  type FileCash = MutableMap[String, List[CacheEntry]]

  protected val program_cache = MutableMap[AST, MutableMap[CacheableMember, List[Member]]]()

  /** This method transforms a program and returns verification results based on the cache's current state.
    *
    * The input and out put program should be equivalent with respect to verification, but they might
    * differ in their AST. In particular, the output program should will be transformed in such a way that
    * verifying it is faster than verifying the input program.
    *
    * The method also returns previously cached verification result for members of the AST that hit the cache.
    * */
  def applyCache(
        file_key: String,
        input_prog: AST): (AST, List[CacheEntry]) = {

    val cache_entries: ListBuffer[CacheEntry] = ListBuffer()
    val concerningsToCache: ListBuffer[CacheableMember] = ListBuffer()
    val concerningsToVerify: ListBuffer[CacheableMember] = ListBuffer()

    //read errors from cache
    val cachable_members = input_prog.decompose()

    val prog_dependencies = program_cache.find({
      case (k, _) => k.equals(input_prog)
    })

    prog_dependencies match {
      case Some((_, dep_map)) =>
        cachable_members.foreach(cm => {
          val dependencies = dep_map(cm)
          get(file_key, cm, dependencies) match {
            case Some(matched_entry) =>
              concerningsToCache += cm.transform
              cache_entries += matched_entry
            case None =>
              //Nothing in cache, request verification
              concerningsToVerify += cm
          }
        })
      case None =>
        val dep_map = MutableMap[CacheableMember, List[Member]]()
        cachable_members.foreach(cm => {
          val dependencies = cm.getDependencies(input_prog)
          dep_map += (cm -> dependencies)
          get(file_key, cm, dependencies) match {
            case Some(matched_entry) =>
              concerningsToCache += cm.transform
              cache_entries += matched_entry
            case None =>
              //Nothing in cache, request verification
              concerningsToVerify += cm
          }
        })
        program_cache += (input_prog -> dep_map)
    }

    val all_concernings: List[CacheableMember] = concerningsToCache.toList ++ concerningsToVerify.toList
    val output_prog: AST = input_prog.compose(all_concernings)
    (output_prog, cache_entries.toList)
  }

  /** Utility function to retrieve entries for single members.
    * */
  final def get(
                 file_key: String,
                 key: CacheableMember,
                 dependencies: List[Member]): Option[CacheEntry] = {

    val concerning_hash = key.hash
    val dependencies_hash = dependencies.map(_.hash).mkString(" ")
    val dependency_hash = CacheHelper.buildHash(concerning_hash + dependencies_hash)
    assert(concerning_hash != null)

    for {
      fileCache <- _cache.get(file_key)
      cacheEntries <- fileCache.get(concerning_hash)
      validEntry <- cacheEntries.find(_.depencyHash == dependency_hash)
    } yield validEntry


  }

  /** This function updates the cache's state by adding/updating entries.
    *
    * All cacheable member that were (re-)verified in this verification attempt must be cached. This obviously
    * requires the cacheable member itself as well as the content that's to be stored.
    *
    * Additionally, a list of dependencies must be passed. This list is used to compute the dependency hash,
    * which is used to determine the validity of a cache entry.
    * */
  def update(
              file_key: String,
              key: CacheableMember,
              dependencies: List[Member],
              content: CacheContent): List[CacheEntry] = {

    val concerning_hash = key.hash
    val dependencies_hash = dependencies.map(_.hash).mkString(" ")
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

  /** Resets the cache for a particular file.
    * */
  def forgetFile(file_key: String): Option[String] = {
    _cache.remove(file_key) match {
      case Some(_) => Some(file_key)
      case None => None
    }
  }

  /** Resets the entire cache.
    * */
  def resetCache(): Unit = {
    _cache.clear()
  }
}

// ===== AUXILIARY TRAITS ==================================================================

/** This class represents a cache entry.
  *
  * An entry contains three pieces of information:
  *
  *  - Which cacheable AST member the entry is for.
  *  - What the cache will store for that member
  *  - A dependency hash of when the member at the time it is stored.
  *
  *  The dependency hash reflects the state of the dependencies at the time when the entry was created.
  *  If at some point a members dependency hash is no longer equal to the one stored here, the
  *  entry is no longer valid.
  * */
class CacheEntry(val concerning: CacheableMember, val cacheContent: CacheContent, val depencyHash: String) {
}

/** This trait is a generic wrapper for cache content.
  *
  * This trait must be extended in order to specify what the cache will hold.
  * */
trait CacheContent {}

/** This trait is a generic wrapper for an AST Members.
  *
  * Every Ast Member must be hashable. I.e., there must exist a hash function for a Member that
  * hashes it to a String.
  * */
trait Member {
  def hash(): String
}

/** This trait is a generic wrapper for AST Members that are cacheable.
  *
  * A cacheable Member, contrary to a normal member, must provide additional functionality. This comes
  * in the form of the methods transform and getDependencies:
  * */
trait CacheableMember extends Member {

  /** A member that has been retrieved from the cache should not be reverified. Therefore, a member must
    * have an alternative representation that allows the verifier to distinguish if from members that
    * missed the cache and need reverification
    * */
  def transform: CacheableMember

  /** Must return a list of members which this member depends on, in terms of verification.
    *
    * A member may hit the cache, but the attached verification results might be invalid. Reason for this
    * is that other members in the program have changed in such a way that influences this member. These
    * influencing members are called dependencies and need to be checked when retrieving a member's attached
    * result from cache.
    * */
  def getDependencies(ast: AST): List[Member]
}

/** This trait is a generic wrapper for an AST.
  *
  * Potentially, only a subset of Member in an AST will be Cacheable. In order for the
  * cache to know which these are, an AST must implement the decompose method.
  *
  * After content has been retrieved from the cache, the corresponding members must be transformed, so as to
  * not be reverified. These members must be reintroduced into the AST, which is done by implementing the
  * compose method
  * */
trait AST {

  /** Must return an AST given a list of transformed cacheable members  which are cacheable in this AST.
    * */
  def compose(cs: List[CacheableMember]): AST

  /** Must return a list of members which are cacheable in this AST.
    * */
  def decompose(): List[CacheableMember]

  def equals(other: AST): Boolean
}


