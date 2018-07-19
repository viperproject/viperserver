/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package viper.server

import ch.qos.logback.classic.Logger
import org.slf4j.LoggerFactory
import viper.silver.ast.{Forall, Method, Node, Program}
import viper.silver.verifier.{AbstractVerificationError, errors}

object ViperCache {
  private val cache = collection.mutable.Map[String, collection.mutable.Map[String, CacheEntry]]()
  override def toString: String = cache.toString

  private var _backendSpecificCache: Boolean = false

  def logger: Logger = LoggerFactory.getLogger(getClass.getName).asInstanceOf[Logger]

  def initialize(backendSpecificCache: Boolean): Unit = {
    _backendSpecificCache = backendSpecificCache
  }

  def contains(backendName: String, file: String, m: Method): Boolean = {
    get(backendName, file, m).isDefined
  }

  def get(backendName: String, file: String, m: Method): Option[CacheEntry] = {
    assert(m.entityHash != null)
    val key = getKey(backendName, file)
    cache.get(key) match {
      case Some(fileCache) =>
        fileCache.get(m.entityHash)
      case None => None
    }
  }

  def update(backendName: String, file: String, p: Program, m: Method, errors: List[AbstractVerificationError]): Unit = {
    assert(m.entityHash != null)
    val key = getKey(backendName, file)
    cache.get(key) match {
      case Some(fileCache) =>
        try {
          val localizedErrors = errors.map(err => LocalizedError(err, getAccessPath(err.offendingNode, m), getAccessPath(err.reason.offendingNode, m), backendName))
          fileCache += (m.entityHash -> new CacheEntry(localizedErrors, p.dependencyHashMap(m)))
        } catch {
          case e: Exception =>
            logger.warn("Error getting the accessPath, the errors cannot be stored in the cache: " + e )
        }
      case None =>
        cache += (key -> collection.mutable.Map[String, CacheEntry]())
        update(backendName, file, p, m, errors)
    }
  }

  def forgetFile(backendName: String, file: String): Option[String] = {
    val key = getKey(backendName, file)
    cache.remove(key) match {
      case Some(_) => Some(key)
      case None => None
    }
  }

  private def getKey(backendName: String, file: String): String = (if (_backendSpecificCache) backendName else "") + file

  def resetCache(): Unit = {
    cache.clear()
  }

  def getAccessPath(nodeToFind: Node, m: Method): List[Int] = {
    val accessPath = computeAccessPath(nodeToFind, m)
    accessPath match {
      case Some(path) => path
      case None => throw new Exception("Cache: Error determining the access path, the offending Node has not been found in the method " + m.name)
    }
  }

  private def posEquals(nodeToFind: Node, curr: Node): Boolean = {
    if (nodeToFind.getClass != curr.getClass) return false
    curr match {
      case c: errors.ErrorNode => nodeToFind.asInstanceOf[errors.ErrorNode].pos == c.pos
      case _ => false
    }
  }

  private def computeAccessPath(nodeToFind: Node, curr: Node): Option[List[Int]] = {
    if (posEquals(nodeToFind, curr)) {
      return Some(List())
    }

    //specialCase for AutoTriggers
    if (nodeToFind.isInstanceOf[Forall] && curr.isInstanceOf[Forall]) {
      if (posEquals(nodeToFind, curr.asInstanceOf[Forall].autoTrigger)) {
        return Some(List())
      }
    }

    val subNodes = curr.subnodes
    subNodes.zipWithIndex.foreach {
      case (node: Node, index: Int) =>
        val res: Option[List[Int]] = computeAccessPath(nodeToFind, node)
        res match {
          case Some(accessPath) => return Some(index :: accessPath)
          case None => None
        }
    }
    None
  }

  def getNode(root: Node, accessPath: List[Int], oldNode: Node): Option[Node] = {
    var curr = root
    accessPath.foreach(index => {
      if (curr.subnodes.length > index) {
        curr = curr.subnodes(index)
      } else {
        return None
      }
    })
    if (curr.getClass == oldNode.getClass) {
      return Some(curr)
    }
    None
  }
}

class CacheEntry(val errors: List[LocalizedError], val dependencyHash: String) {
  override def toString = s"CacheEntry(errors=$errors, dependencyHash=$dependencyHash)"
}

case class LocalizedError(error: AbstractVerificationError, accessPath: List[Int], reasonAccessPath: List[Int], backendName: String) {
  override def toString = s"LocalizedError(error=$error, accessPath=$accessPath, reasonAccessPath=$reasonAccessPath, backendName=$backendName)"
}

class AccessPath(val accessPath: List[Number]) {
  override def toString = s"AccessPath(accessPath=$accessPath)"
}