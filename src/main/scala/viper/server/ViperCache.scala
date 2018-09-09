/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package viper.server

import ch.qos.logback.classic.Logger
import viper.silver.ast._
import viper.silver.utility.CacheHelper
import viper.silver.verifier.{AbstractVerificationError, errors}

import scala.collection.mutable

object ViperCache {
  /**
    * At the top level we store file caches via keys (backend, file) -> String.
    *
    * Each file cache is a map of the form (hash -> List[CacheEntry]), because
    *  we potentially store many verification results for each method.
    */
  private val _cache = collection.mutable.Map[String, collection.mutable.Map[String, List[CacheEntry]]]()

  /**
    * This map contains priorly computed hashes of AST nodes for which the hash is not already computed lazily.
    * This optimization might be beneficial because not all instances of [[Node]] are [[Hashable]].
    */
  private val _node_hash_memo = mutable.Map.empty[String, mutable.Map[Node, String]]

  override def toString: String = _cache.toString

  private var _backendSpecificCache: Boolean = false

  var _logger: Logger = _
  def logger: Logger = _logger

  def initialize(logger: Logger, backendSpecificCache: Boolean): Unit = {
    _backendSpecificCache = backendSpecificCache
    _logger = logger
  }

  def contains(backendName: String, file: String, m: Method): Boolean = {
    get(backendName, file, m).nonEmpty
  }

  def get(backendName: String, file: String, m: Method): List[CacheEntry] = {
    assert(m.entityHash != null)
    val key = getKey(backendName, file)
    _cache.get(key) match {
      case Some(fileCache) =>
        fileCache.get(m.entityHash) match {
          case Some(cache_entry_list) => cache_entry_list
          case None => Nil
        }
      case None => Nil
    }
  }

  def update(backendName: String, file: String, p: Program, m: Method, errors: List[AbstractVerificationError]): List[CacheEntry] = {
    assert(m.entityHash != null)
    implicit val key: String = getKey(backendName, file)
    _cache.get(key) match {
      case Some(fileCache) =>
        try {
          val localizedErrors = errors.map(err =>
            LocalizedError(err,
              getAccessPath(err.offendingNode, p),
              getAccessPath(err.reason.offendingNode, p), backendName))
          val entry = new CacheEntry(localizedErrors, p.dependencyHashMap(m))
          val updated_cache_entry_list = entry :: fileCache.getOrElse(m.entityHash, Nil)
          fileCache(m.entityHash) = updated_cache_entry_list
          updated_cache_entry_list
        } catch {
          case e: Exception =>
            logger.error("Error getting the access path; the errors could not be stored in the cache: " + e )
            fileCache(m.entityHash)
        }
      case None =>
        _cache += (key -> collection.mutable.Map[String, List[CacheEntry]]())
        update(backendName, file, p, m, errors)
    }
  }

  def forgetFile(backendName: String, file: String): Option[String] = {
    val key = getKey(backendName, file)
    _node_hash_memo.remove(key)
    _cache.remove(key) match {
      case Some(_) => Some(key)
      case None => None
    }
  }

  private def getKey(backendName: String, file: String): String = (if (_backendSpecificCache) backendName else "") + file

  def resetCache(): Unit = {
    _node_hash_memo.clear()
    _cache.clear()
  }

  def getAccessPath(nodeToFind: Node, p: Program)(implicit key: String): List[String] = {
    logger.trace(s"Computing access path for node ${nodeToFind.toOneLinerStr()}...")
    val accessPath = computeAccessPath(nodeToFind, p)
    accessPath match {
      case Some(path) => path
      case None => throw new Exception(s"Cache: error determining the access path, the offending node ($nodeToFind) has not been found in the program.")
    }
  }

  private def posEquals(nodeToFind: Node, curr: Node): Boolean = {
    if (nodeToFind.getClass != curr.getClass) return false
    curr match {
      case c: errors.ErrorNode => nodeToFind.asInstanceOf[errors.ErrorNode].pos == c.pos
      case _ => false
    }
  }

  private def computeAccessPath(nodeToFind: Node, curr: Node)(implicit key: String): Option[List[String]] = {
    if (posEquals(nodeToFind, curr)) {
      Some(Nil)
    } else if (nodeToFind.isInstanceOf[Forall] && curr.isInstanceOf[Forall]
               && posEquals(nodeToFind, curr.asInstanceOf[Forall].autoTrigger)) {
      // Special case for auto triggers
      Some(Nil)
    } else {
      logger.trace(s"curr = ${curr.toOneLinerStr()}; curr.subnodes = ${curr.subnodes.map(_.toOneLinerStr())}")
      curr.subnodes.foreach {
        node: Node =>
          computeAccessPath(nodeToFind, node) match {
            case Some(access_path) =>
              val hash = getHashForNode(node)
              logger.trace(s" (${node.toOneLinerStr()} -> ${hash.hashCode.toHexString})")
              return Some(hash :: access_path)
            case None => None
          }
      }
      None
    }
  }

  /**
    * This method is used for computing unique-ish hashes of AST nodes.
    *
    * It is important that the hash depends only on the part of the AST node
    *  that will **not** be cached. Otherwise, we do not have the guarantee
    *  that the mapping [[Node]]->[[String]] remains constant before and after caching.
    *  Currently, we only cache method bodies.
    *
    * This method performs an optimization: we need to get a hash for any node,
    * but the hash is computed lazily only for [[Hashable]] nodes.
    *
    * Unfortunately, the type [[Node]] is used in the project in many places where
    *  [[Hashable]] would be preferable. For such nodes, we need to add additional
    *  memoization (normally, memoization is done via trait [[Hashable]]'s lazy val).
    *
    * The second argument list is used for specifying external keys as (backend, file).
    *  This is needed for removing separate parts of the hash table.
    *  @see [[forgetFile]].
    */
  private def getHashForNode(node: Node)(implicit key: String): String = node match {
    case m: Method => removeBody(m).entityHash
    case hn: Hashable => hn.entityHash
    case n =>
      _node_hash_memo.get(key) match {
        case Some(memo) => memo.get(n) match {
          case Some(hash) => hash
          case None =>
            if ( memo.size > 100 || _node_hash_memo.size > 100 ) {
              val msg = s"[WARNING] ViperCache has memoized more than 100 non-Hashable nodes." +
                s" Consider optimizing the code."
              logger.warn(msg)
              println(msg)
            }
            val hash = CacheHelper.computeEntityHash("", node)
            _node_hash_memo(key)(n) = hash
            hash
        }
        case None =>
          _node_hash_memo(key) = mutable.Map.empty[Node, String]
          getHashForNode(n)
      }
  }

  def removeBody(m: Method): Method = m.copy(body = None)(m.pos, ConsInfo(m.info, Cached), m.errT)

  private def str(n: Node)(implicit key: String) = s"(${n.toOneLinerStr()} -> ${getHashForNode(n).hashCode.toHexString})"
  private def hex(h: String) = h.hashCode.toHexString

  def getNode(backendName: String, file: String, p: Program, accessPath: List[String], oldNode: Node): Option[Node] = {
    implicit val key: String = getKey(backendName, file)
    logger.trace(s"looking for last node on access path ${accessPath.map(hex)}...")
    var curr: Node = p
    accessPath.foreach(hash => {
      logger.trace(s" ... curr = ${str(curr)}")
      logger.trace(s" ... considering hash ${hex(hash)} among subnodes ${curr.subnodes.map(str)}...")
      curr.subnodes.find { sub => getHashForNode(sub) == hash } match {
        case Some(hashed_subnode) =>
          // hash corresponds to a subnode of curr.
          curr = hashed_subnode
        case None =>
          // no subnode of curr corresponds to the hash
          return None
      }
    })
    if (curr.getClass == oldNode.getClass) {
      logger.trace(s" ==> found node: (${curr.toOneLinerStr()} -> ${getHashForNode(curr).hashCode.toHexString})")
      Some(curr)
    } else {
      logger.trace(s" ==> node not found!")
      None
    }
  }
}

class CacheEntry(val errors: List[LocalizedError], val dependencyHash: String) {
  override def toString = s"CacheEntry(errors=$errors, dependencyHash=${dependencyHash.hashCode.toHexString})"
}

case class LocalizedError(error: AbstractVerificationError, accessPath: List[String], reasonAccessPath: List[String], backendName: String) {
  override def toString = s"LocalizedError(error=${error.loggableMessage}, accessPath=${accessPath.map(_.hashCode.toHexString)}, reasonAccessPath=${reasonAccessPath.map(_.hashCode.toHexString)}, backendName=$backendName)"
}

class AccessPath(val accessPath: List[Number]) {
  override def toString = s"AccessPath(accessPath=${accessPath.map(_.hashCode.toHexString)})"
}