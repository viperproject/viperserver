// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2020 ETH Zurich.

package viper.server.vsi

import viper.silver.utility.CacheHelper

import java.security.MessageDigest
import java.time.Instant
import scala.collection.mutable.ListBuffer

/** The goal of this generic caching trait is to provide:
  *
  * A) a storing system that can decide whether or not stored resources are valid.
  *
  * in order to implement:
  *
  * B) A simple caching API
  *
  * A verification server should - very bluntly expressed - take a program and return a
  * (potentially empty) set of errors. This process is assumed to be deterministic. I.e.,
  * verifying the same program twice results in the exact same verification result. Of course,
  * changing the program is likely to change the verification outcome. However, it may well be
  * that the outcome will not completely change. E.g., if a program consists of two totally
  * independent methods, changing the first may have absolutely no effect on the verification
  * results of the second. In such a situation, resources can be economized by storing the
  * results, reverifying only the first method and not reverifying the second. This requires a
  * storage system that can:
  *
  *   1) store verification results for individual program members
  *   2) decide whether or not stored results are still valid if the program is changed.
  *
  * Once such a system is in place it can be used to implement a simple API. This API should take
  * a program P, analyse it, transforms it and return a program P' as well as a set of
  * verification results. This program should thereby have been transformed in such a way that a
  * verifier may not perform unnecessary verifications on its members.
  * */
abstract class Cache {

  override def toString: String = _cache.toString

  /** The cache infrastructure is a nested, mutable Map. The maps can be described in terms of
    * their key and values as follows:  (FileName -> (hashString -> cacheEntries)).
    *
    * The inner map is referred to as the fileCache. As the name indicates, it stores, for each
    * file, a number of hashes and corresponding cache entries.
    */
  type FileKey = String
  type MethodHash = String
  type FileCache = Map[MethodHash, List[CacheEntry]]
  type Cache = Map[FileKey, FileCache]
  protected var _cache: Cache = Map()

  type ProgramHash = String
  type DependencyHash = String
  type DependendyMap = Map[MethodHash, DependencyHash]
  protected var _program_cache: Map[ProgramHash, DependendyMap] = Map()

  /** This method transforms a program and returns verification results based on the cache's
    * current state.
    *
    * The input and out put program should be equivalent with respect to verification, but they
    * might differ in their AST. In particular, the output program should will be transformed in
    * such a way that verifying it is faster than verifying the input program.
    *
    * The method also returns previously cached verification result for members of the AST that
    * hit the cache.
    * */
  def retrieve(
        file_key: String,
        input_prog: Ast): (Ast, List[CacheEntry]) = {

    val cache_entries: ListBuffer[CacheEntry] = ListBuffer()
    val concerningsToCache: ListBuffer[CacheableMember] = ListBuffer()
    val concerningsToVerify: ListBuffer[CacheableMember] = ListBuffer()

    //read errors from cache
    val cachable_members = input_prog.decompose()

    val input_prog_hex = MessageDigest.getInstance("SHA-1")
      .digest(input_prog.toString.getBytes("UTF-8"))
      .map("%02x".format(_)).mkString

    val prog_dependencies = _program_cache.get(input_prog_hex)

    val dep_map = prog_dependencies match {
      case Some(dep_map) => dep_map
      case None =>
        var dep_map = Map[String, String]()
        cachable_members.foreach(cm => {
          val concerning_hash = cm.hash()
          val dependency_hash = CacheHelper.buildHash(concerning_hash + cm.getDependencies(input_prog).map(_.hash()).mkString(" "))
          dep_map = dep_map + (concerning_hash -> dependency_hash)
        })
        _program_cache = _program_cache + (input_prog_hex -> dep_map)
        dep_map
    }

    cachable_members.foreach(cm => {
      val concerning_hash = cm.hash()
      val dependency_hash = dep_map(concerning_hash)

      get(file_key, concerning_hash, dependency_hash) match {
        case Some(matched_entry) =>
          matched_entry.lastAccessed = Instant.now()
          concerningsToCache += cm.transform
          cache_entries += matched_entry
        case None =>
          //Nothing in cache, request verification
          concerningsToCache += cm
      }
    })

    val all_concernings: List[CacheableMember] = concerningsToCache.toList ++ concerningsToVerify.toList
    val output_prog: Ast = input_prog.compose(all_concernings)
    (output_prog, cache_entries.toList)
  }

  /** Utility function to retrieve entries for single members.
    * */
  final def get(file_key: String,
                concerning_hash: String,
                dependency_hash: String): Option[CacheEntry] = {
    assert(concerning_hash != null)
    for {
      fileCache <- _cache.get(file_key)
      cacheEntries <- fileCache.get(concerning_hash)
      validEntry <- cacheEntries.find(_.dependencyHash == dependency_hash)
    } yield validEntry
  }

  /** This function updates the cache's state by adding/updating entries.
    *
    * All cacheable member that were (re-)verified in this verification attempt must be cached.
    * This obviously requires the cacheable member itself as well as the content that's to be
    * stored.
    *
    * Additionally, a list of dependencies must be passed. This list is used to compute the
    * dependency hash, which is used to determine the validity of a cache entry.
    * */
  def update(
              file_key: String,
              key: CacheableMember,
              dependencies: List[Member],
              content: CacheContent): List[CacheEntry] = {

    val concerning_hash = key.hash()
    val dependencies_hash = dependencies.map(_.hash()).mkString(" ")
    val dependency_hash = CacheHelper.buildHash(concerning_hash + dependencies_hash)
    val new_entry: CacheEntry = CacheEntry(key.hash(), content, dependency_hash)

    assert(concerning_hash != null)

    _cache.get(file_key) match {
      case Some(fileCache) =>
        val existing_entries = fileCache.getOrElse(concerning_hash, Nil)
        val updated_cacheEntries = new_entry :: existing_entries

        val updatedFileCache = fileCache + (concerning_hash -> updated_cacheEntries)
        _cache = _cache + (file_key -> updatedFileCache)
        updated_cacheEntries
      case None =>
        //if file not in cache yet, create new map entry for it. Recall the function.
        _cache = _cache + (file_key -> Map())
        update(file_key, key, dependencies, content)
    }
  }

  /** Resets the cache for a particular file.
    * */
  def forgetFile(file_key: String): Option[String] = {
    val elem = _cache.get(file_key) match {
      case Some(_) => Some(file_key)
      case None => None
    }
    _cache -= file_key
    elem
  }

  /** Resets the entire cache.
    * */
  def resetCache(): Unit = {
    _program_cache = Map()
    _cache = Map()
  }
}


/** This case class represents a cache entry.
  *
  * An must contain three pieces of information:
  *
  *    - Which cacheable AST member the entry is for.
  *    - What the cache will store for that cacheable member.
  *    - A dependency hash for that cacheable member at the time it is stored.
  *
  *  The dependency hash reflects the state of the dependencies at the time when the entry was
  *  created. If, at some point, a members dependency hash is no longer equal to the one stored
  *  here, the entry is no longer valid.
  * */
case class CacheEntry(concerningHash: String, content: CacheContent, dependencyHash: String, created: Instant = Instant.now(), var lastAccessed: Instant = Instant.now())


// ===== AUXILIARY TRAITS ==================================================================


/** This trait is a generic wrapper for cache content. It must be extended in order to specify
  * what the cache will hold.
  *
  * Typically, this will hold client-specific verification results.
  * */
trait CacheContent


/** This trait is a generic wrapper for an AST Members. It must be extended in order to specify
  * what members an AST is made of.
  * */
trait Member {

  /** Must be implemented to return a hash String for this Member.
    * */
  def hash(): String
}


/** This trait is a generic wrapper for the subset of AST Members that are cacheable. It must be
  * extended in order to specify which AST members are cacheable.
  *
  * Some languages for which caching is implemented in a verification server might not provide
  * caching for each of its members. In Viper, for example, only members of type Methods are
  * cacheable.
  *
  * A cacheable member, contrary to a normal member, must provide additional functionality. This
  * comes in the form of the (abstract) methods transform and getDependencies.
  * */
trait CacheableMember extends Member {

  /** Must be implemented to transform this CacheableMember such that it is not reverified with
    * the backends used.
    *
    * A member for which verification results have been retrieved from the cache should not be
    * reverified. For this, a member must be transformed. The transformation should be done such
    * that it allows the verifier to distinguish between members that hit the cache (and do not
    * need verification) and members that missed the cache (and therefore do need reverification).
    * */
  def transform: CacheableMember

  /** Must be implemented to return a list of members which this CacheableMember depends on, in
    * terms of verification.
    *
    * A member may hit the cache, but the attached verification results might be invalid. Reason
    * for this is that other members in the program might have changed in such a way that
    * they influenced this member's verification outcome. These influencing members are called
    * dependencies and need to be checked when retrieving a member's attached result from cache.
    * */
  def getDependencies(ast: Ast): List[Member]
}


/** This trait is a generic wrapper for an AST.
  * */
trait Ast {

  /** Must be implemented to return and Ast given a list of transformed CacheableMembers and this
    * Ast.
    *
    * Potentially, only a subset of Member in an AST will be cacheable. In order for the cache to
    * know which these are, an Ast must implement a decompose method.
    * */
  def compose(cs: List[CacheableMember]): Ast

  /** Must be implemented to return a list of CacheableMembers for this Ast.
    *
    * * After content has been retrieved from the cache, the corresponding members must be
    * transformed, so as to not be reverified. These members must be reintroduced into the AST,
    * which is done by implementing the compose method.
    * */
  def decompose(): List[CacheableMember]

  /** Must be implemented to decide whether an Ast is equal to this Ast in terms of verification.
    *
    * If it is known that a program is equal to one previously cached, the results should be
    * retrievable without delay. In particular, it should not be necessary to compute the
    * dependency list for each CacheableMembers when retrieving their results from the cache, as
    * equal programs imply equal dependency lists.
    *
    * Implementing this method in a non-trivial way is obviously difficult. Clients who do not
    * want to bother with this feature can simply return false for any pair of programs.
    * */
  def equals(other: Ast): Boolean
}