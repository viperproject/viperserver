///**
//  * This Source Code Form is subject to the terms of the Mozilla Public
//  * License, v. 2.0. If a copy of the MPL was not distributed with this
//  * file, You can obtain one at http://mozilla.org/MPL/2.0/.
//  *
//  * Copyright (c) 2011-2019 ETH Zurich.
//  */
//
//package viper.server.core
//
//import ch.qos.logback.classic.Logger
//import viper.silver.ast._
//import viper.silver.utility.CacheHelper
//import viper.silver.verifier.{AbstractVerificationError, errors}
//
//import scala.collection.mutable
//
//object ViperCache {
////  /**
////    * At the top level we store file caches via keys (backend, file) -> String.
////    *
////    * Each file cache is a map of the form (hash -> List[CacheEntry]), because
////    *  we potentially store many verification results for each method.
////    */
//  /** The cache infrastructure is a nested MutableMap. They can be described as follows:
//    * (FileName -> (EntityHashString -> CacheEntries))
//    *
//    * The inner map is usually referred to as the fileCache. As the name indicates, it stores,
//    * for each file, a number of hashes and corresponding cache entries.
//    */
//  private val _cache = collection.mutable.Map[String, collection.mutable.Map[String, List[CacheEntry]]]()
//
//  /**
//    * This map contains priorly computed hashes of AST nodes for which the hash is not already computed lazily.
//    * This optimization might be beneficial because not all instances of [[Node]] are [[Hashable]].
//    * */
//  private val _node_hash_memo = mutable.Map.empty[String, mutable.Map[Node, String]]
//
//  override def toString: String = _cache.toString
//
//
//  //Specific
//  private var _backendSpecificCache: Boolean = false
//
//  //Specific
//  var _logger: Logger = _
//  def logger: Logger = _logger
//
//  //Specific
//  def initialize(logger: Logger, backendSpecificCache: Boolean): Unit = {
//    _backendSpecificCache = backendSpecificCache
//    _logger = logger
//  }
//
//  // ===== (GENERIC) UTILITY METHODS ==================================================================
//
//  /** Key constructor for the cache.
//    *
//    *
//    *
//    * */
//  private def getKey(backendName: String, file: String): String = (if (_backendSpecificCache) backendName else "") + file
//
//  /** Return contents of the cache corresponding to a file and method.
//    *
//    * If the file has cache associated and the methods hash is in that cache return the cached entries
//    * Else, return empty list.
//    * */
//  def get(backendName: String, file: String, m: Method): List[CacheEntry] = {
//    assert(m.entityHash != null)
//    val key = getKey(backendName, file)
//
//    _cache.get(key) match {
//      case Some(fileCache) =>
//        fileCache.get(m.entityHash) match {
//          case Some(cache_entry_list) => cache_entry_list
//          case None => Nil
//        }
//      case None => Nil
//    }
//  }
//
//  /** Utility function to check whether the cache contains cache entries for a given file
//    * and.
//    * */
//  def contains(backendName: String, file: String, m: Method): Boolean = {
//    get(backendName, file, m).nonEmpty
//  }
//
//  /** Updates the cache's content
//    *
//    * Given a file, add a new cache entry to its fileCache. Note that a cacheEntry is specific to a method.
//    * This means that the update will either add a new pair (Hash, cacheEntries) or add a new one. The
//    * cacheEntry then contains (a mapped version of) that method's verification errors.
//    * */
//  def update(backendName: String, file: String, p: Program, m: Method, errors: List[AbstractVerificationError]): List[CacheEntry] = {
//    assert(m.entityHash != null)
//    implicit val key: String = getKey(backendName, file)
//
//    _cache.get(key) match {
//      case Some(fileCache) =>
//        // If a fileChace exists for given file ...
//        try {
//          // ... map the errors to localizedErrors and wrap them into a cacheEntry
//          val localizedErrors = errors.map(err =>
//            LocalizedError(err,
//              getAccessPath(err.offendingNode, p),
//              getAccessPath(err.reason.offendingNode, p),
//              backendName))
//          val new_entry = new CacheEntry(localizedErrors, p.dependencyHashMap(m))
//
//          //get exisitng list of entries (or an empty list, if none exist yet) and prepend new entry
//          val existing_entries = fileCache.getOrElse(m.entityHash, Nil)
//          val updated_cacheEntries = new_entry :: existing_entries
//
//          //set new hash/cacheEntries pair in map
//          fileCache(m.entityHash) = updated_cacheEntries
//          updated_cacheEntries
//        } catch {
//          case e: Exception =>
//            logger.error("Error getting the access path; the errors could not be stored in the cache: " + e )
//            fileCache(m.entityHash)
//        }
//      case None =>
//        //if file not in cache yet, create new map entry for it, restart the function
//        _cache += (key -> collection.mutable.Map[String, List[CacheEntry]]())
//        update(backendName, file, p, m, errors)
//    }
//  }
//
// /** Removes a specific a file from the cache
//   *
//   * I.e., removes the file and its corresponding file cache
//   * */
//  def forgetFile(backendName: String, file: String): Option[String] = {
//    val key = getKey(backendName, file)
//    _node_hash_memo.remove(key)
//
//    _cache.remove(key) match {
//      case Some(_) => Some(key) //If file removed return that file's name
//      case None => None
//    }
//  }
//
//  /** Clear all the caches.
//    * */
//  def resetCache(): Unit = {
//    _node_hash_memo.clear()
//    _cache.clear()
//  }
//
//  // ===== (SPECIFIC) UTILITY METHODS ==================================================================
//
//  /**
//    * This method is used for computing unique-ish hashes of AST nodes.
//    *
//    * It is important that the hash depends only on the part of the AST node
//    *  that will **not** be cached. Otherwise, we do not have the guarantee
//    *  that the mapping [[Node]]->[[String]] remains constant before and after caching.
//    *  Currently, we only cache method bodies.
//    *
//    * This method performs an optimization: we need to get a hash for any node,
//    * but the hash is computed lazily only for [[Hashable]] nodes.
//    *
//    * Unfortunately, the type [[Node]] is used in the project in many places where
//    *  [[Hashable]] would be preferable. For such nodes, we need to add additional
//    *  memoization (normally, memoization is done via trait [[Hashable]]'s lazy val).
//    *
//    * The second argument list is used for specifying external keys as (backend, file).
//    *  This is needed for removing separate parts of the hash table.
//    *  @see [[forgetFile]].
//    */
//  private def getHashForNode(node: Node)(implicit key: String): String = node match {
//    case m: Method => removeBody(m).entityHash
//    case hn: Hashable => hn.entityHash
//    case n =>
//      _node_hash_memo.get(key) match {
//        case Some(memo) => memo.get(n) match {
//          case Some(hash) => hash
//          case None =>
//            if ( memo.size > 100 || _node_hash_memo.size > 100 ) {
//              val msg = s"[WARNING] ViperCache has memoized more than 100 non-Hashable nodes." +
//                s" Consider optimizing the code."
//              logger.warn(msg)
//              println(msg)
//            }
//            val hash = CacheHelper.computeEntityHash("", node)
//            _node_hash_memo(key)(n) = hash
//            hash
//        }
//        case None =>
//          _node_hash_memo(key) = mutable.Map.empty[Node, String]
//          getHashForNode(n)
//      }
//  }
//
//  /** Checks if two (error) nodes have equal position
//    *
//    * Note: positions can be specified in various forms (line/col, identifier, etc).
//    * */
//  private def posEquals(nodeToFind: Node, curr: Node): Boolean = {
//    //Nodes must be of equal type to have equal
//    if (nodeToFind.getClass != curr.getClass) return false
//
//    curr match {
//      // If current node is of type ErrorNode, positions can be compared.
//      case en: errors.ErrorNode => nodeToFind.asInstanceOf[errors.ErrorNode].pos == en.pos
//      case _ => false
//    }
//  }
//
//  /** Computes an path from one Node to another.
//    *
//    * The returned strings are node hashes. The List of string therefore is the list of nodes (hashes)
//    * that are found on the path from the current node to the sought node.
//    * */
//  private def computeAccessPath(nodeToFind: Node, curr: Node)(implicit key: String): Option[List[String]] = {
//    if (posEquals(nodeToFind, curr)) {
//      // Both nodes are equal, return an empty Path (I.e., some empty list)
//      Some(Nil)
//    } else if (nodeToFind.isInstanceOf[Forall] && curr.isInstanceOf[Forall]
//               && posEquals(nodeToFind, curr.asInstanceOf[Forall].autoTrigger)) {
//      // Special case for auto triggers
//      Some(Nil)
//    } else {
//      // If the nodes are not equal ...
//      logger.trace(s"curr = ${curr.toOneLinerStr()}; curr.subnodes = ${curr.subnodes.map(_.toOneLinerStr())}")
//      curr.subnodes.foreach { node: Node =>
//        // Go through all the node's children and recursively compute the path to the sought node from there
//        computeAccessPath(nodeToFind, node) match {
//          case Some(access_path) =>
//            // If a path is returned, the right subnodes was found. Compute hash of current node
//            // and append it to the path list.
//            val hash = getHashForNode(node)
//            logger.trace(s" (${node.toOneLinerStr()} -> ${hash.hashCode.toHexString})")
//            return Some(hash :: access_path)
//          case None => None
//        }
//      }
//      None
//    }
//  }
//
//  /** Computes a node's path through the program.
//    *
//    * Note that a program is itseld a (root-) node.
//    * */
//  def getAccessPath(nodeToFind: Node, p: Program)(implicit key: String): List[String] = {
//    logger.trace(s"Computing access path for node ${nodeToFind.toOneLinerStr()}...")
//    val accessPath = computeAccessPath(nodeToFind, p)
//    accessPath match {
//      case Some(path) => path
//      case None => throw new Exception(s"Cache: error determining the access path, the offending node ($nodeToFind) has not been found in the program.")
//    }
//  }
//
//  /** Removes a method's body
//    *
//    * Returns a new copy of the method with body field null.
//    * */
//  def removeBody(m: Method): Method = m.copy(body = None)(m.pos, ConsInfo(m.info, Cached), m.errT)
//
//  private def str(n: Node)(implicit key: String) = s"(${n.toOneLinerStr()} -> ${getHashForNode(n).hashCode.toHexString})"
//  private def hex(h: String) = h.hashCode.toHexString
//
//
//  /** Finds a node in a program by traversing the provided accessPath
//    *
//    * */
//  def getNode(backendName: String, file: String, p: Program, accessPath: List[String], oldNode: Node): Option[Node] = {
//    implicit val key: String = getKey(backendName, file)
//    logger.trace(s"looking for last node on access path ${accessPath.map(hex)}...")
//
//    // start at root and traverse path node (hash) by node (hash)
//    var curr: Node = p
//    accessPath.foreach(hash => {
//      logger.trace(s" ... curr = ${str(curr)}")
//      logger.trace(s" ... considering hash ${hex(hash)} among subnodes ${curr.subnodes.map(str)}...")
//
//      // In the list of the current node's children, find the one who's hash matches the hash
//      // specified by the accesspath.
//      curr.subnodes.find { sub => getHashForNode(sub) == hash } match {
//        case Some(hashed_subnode) =>
//          // hash corresponds to a subnode of curr.
//          curr = hashed_subnode
//        case None =>
//          // no subnode of curr corresponds to the hash
//          return None
//      }
//    })
//
//    // If path traversal successful check that found node and old node's classes match
//    if (curr.getClass == oldNode.getClass) {
//      logger.trace(s" ==> found node: (${curr.toOneLinerStr()} -> ${getHashForNode(curr).hashCode.toHexString})")
//      Some(curr)
//    } else {
//      logger.trace(s" ==> node not found!")
//      None
//    }
//  }
//}
//
///** A cache entry holds a [[LocalizedError]] and a hash [[String]]
//  * */
//class CacheEntry(val errors: List[LocalizedError], val dependencyHash: String) {
//  override def toString = s"CacheEntry(errors=$errors, dependencyHash=${dependencyHash.hashCode.toHexString})"
//}
//
///** A localized error contains the Abstract Verification Error, paths
//  *
//  * */
//case class LocalizedError(error: AbstractVerificationError, accessPath: List[String], reasonAccessPath: List[String], backendName: String) {
//  override def toString = s"LocalizedError(error=${error.loggableMessage}, accessPath=${accessPath.map(_.hashCode.toHexString)}, reasonAccessPath=${reasonAccessPath.map(_.hashCode.toHexString)}, backendName=$backendName)"
//}
//
///** An access path holds a List of Numbers
//  *
//  * */
//class AccessPath(val accessPath: List[Number]) {
//  override def toString = s"AccessPath(accessPath=${accessPath.map(_.hashCode.toHexString)})"
//}
//
