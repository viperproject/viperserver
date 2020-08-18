package viper.server.core

import ch.qos.logback.classic.Logger
import viper.server.vsi.{CacheContent, CacheEntry, VerificationServerInterfaceCache}
import viper.silver.ast.{Cached, ConsInfo, Forall, Hashable, Method, Node, Program}
import viper.silver.utility.CacheHelper
import viper.silver.verifier.{AbstractVerificationError, errors}

import scala.collection.mutable

object NewViperCache extends VerificationServerInterfaceCache {

  private var _backendSpecificCache: Boolean = false
  private val _node_hash_memo = mutable.Map.empty[String, mutable.Map[Node, String]]

  var _logger: Logger = _
  def logger: Logger = _logger

  def initialize(logger: Logger, backendSpecificCache: Boolean): Unit = {
    _backendSpecificCache = backendSpecificCache
    _logger = logger
  }


  type Content = ViperCacheContent
  type Concerning = ViperAst
  def hashFunction(in: ViperAst): String = {
    in.m.entityHash
  }

  override def forgetFile(backendName: String, file: String): Option[String] = {
    val key = getKey(backendName, file)
    _node_hash_memo.remove(key)
    _cache.remove(key) match {
      case Some(_) => Some(key)
      case None => None
    }
  }

  override def resetCache(): Unit = {
    _node_hash_memo.clear()
    _cache.clear()
  }

  override def getKey(file: String, backendName: String): String = {
    (if (_backendSpecificCache) backendName else "") + file
  }

  def createCacheEntry(errors: Content, dh: Hash): ViperCacheEntry = {
    ViperCacheEntry(errors, dh)
  }

  def createCacheContent(backendName: String, file: String,
                         p: Program, m: Method,
                         errors: List[AbstractVerificationError]): ViperCacheContent = {
    implicit val key: String = getKey(backendName, file)
    val loc_errs = errors.map(err =>
      LocalizedError(err,
        getAccessPath(err.offendingNode, p),
        getAccessPath(err.reason.offendingNode, p),
        backendName))
    ViperCacheContent(loc_errs)
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

  /** Checks if two (error) nodes have equal position
    *
    * Note: positions can be specified in various forms (line/col, identifier, etc).
    * */
  private def posEquals(nodeToFind: Node, curr: Node): Boolean = {
    //Nodes must be of equal type to have equal
    if (nodeToFind.getClass != curr.getClass) return false

    curr match {
      // If current node is of type ErrorNode, positions can be compared.
      case en: errors.ErrorNode => nodeToFind.asInstanceOf[errors.ErrorNode].pos == en.pos
      case _ => false
    }
  }

  /** Computes a path from one Node to another.
    *
    * The returned strings are node hashes. The List of string therefore is the list of nodes (hashes)
    * that are found on the path from the current node to the sought node.
    * */
  private def computeAccessPath(nodeToFind: Node, curr: Node)(implicit key: String): Option[List[String]] = {
    if (posEquals(nodeToFind, curr)) {
      // Both nodes are equal, return an empty Path (I.e., some empty list)
      Some(Nil)
    } else if (nodeToFind.isInstanceOf[Forall] && curr.isInstanceOf[Forall]
      && posEquals(nodeToFind, curr.asInstanceOf[Forall].autoTrigger)) {
      // Special case for auto triggers
      Some(Nil)
    } else {
      // If the nodes are not equal ...
      logger.trace(s"curr = ${curr.toOneLinerStr()}; curr.subnodes = ${curr.subnodes.map(_.toOneLinerStr())}")
      curr.subnodes.foreach { node: Node =>
        // Go through all the node's children and recursively compute the path to the sought node from there
        computeAccessPath(nodeToFind, node) match {
          case Some(access_path) =>
            // If a path is returned, the right subnodes was found. Compute hash of current node
            // and append it to the path list.
            val hash = getHashForNode(node)
            logger.trace(s" (${node.toOneLinerStr()} -> ${hash.hashCode.toHexString})")
            return Some(hash :: access_path)
          case None => None
        }
      }
      None
    }
  }

  /** Computes a node's path through the program.
    *
    * Note that a program is itself a (root-) node.
    * */
  def getAccessPath(nodeToFind: Node, p: Program)(implicit key: String): List[String] = {
    logger.trace(s"Computing access path for node ${nodeToFind.toOneLinerStr()}...")
    val accessPath = computeAccessPath(nodeToFind, p)
    accessPath match {
      case Some(path) => path
      case None => throw new Exception(s"Cache: error determining the access path, the offending node ($nodeToFind) has not been found in the program.")
    }
  }

  private def str(n: Node)(implicit key: String) = s"(${n.toOneLinerStr()} -> ${getHashForNode(n).hashCode.toHexString})"

  /** Finds a node in a program by traversing the provided accessPath
    *
    * */
  def getNode(backendName: String, file: String, p: Program, accessPath: List[String], oldNode: Node): Option[Node] = {
    implicit val key: String = getKey(backendName, file)
    logger.trace(s"looking for last node on access path ${accessPath.map(hex)}...")

    // start at root and traverse path node (hash) by node (hash)
    var curr: Node = p
    accessPath.foreach(hash => {
      logger.trace(s" ... curr = ${str(curr)}")
      logger.trace(s" ... considering hash ${hex(hash)} among subnodes ${curr.subnodes.map(str)}...")

      // In the list of the current node's children, find the one who's hash matches the hash
      // specified by the accesspath.
      curr.subnodes.find { sub => getHashForNode(sub) == hash } match {
        case Some(hashed_subnode) =>
          // hash corresponds to a subnode of curr.
          curr = hashed_subnode
        case None =>
          // no subnode of curr corresponds to the hash
          return None
      }
    })

    // If path traversal successful check that found node and old node's classes match
    if (curr.getClass == oldNode.getClass) {
      logger.trace(s" ==> found node: (${curr.toOneLinerStr()} -> ${getHashForNode(curr).hashCode.toHexString})")
      Some(curr)
    } else {
      logger.trace(s" ==> node not found!")
      None
    }
  }

  def removeBody(m: Method): Method = m.copy(body = None)(m.pos, ConsInfo(m.info, Cached), m.errT)
}

// ===== AUXILIARY CLASSES ==================================================================

/** A cache entry holds an errors of type [[LocalizedError]] and hashes of type [[String]]
  * */
case class ViperCacheEntry(content:ViperCacheContent,
                           dependencyHash: String) extends CacheEntry(content, dependencyHash) {

  override def toString = s"CacheEntry(errors=$errors, dependencyHash=${dependencyHash.hashCode.toHexString})"
}

case class ViperCacheContent(errors: List[LocalizedError]) extends CacheContent

/** A localized error contains the Abstract Verification Error, paths
  *
  * */
case class LocalizedError(error: AbstractVerificationError,
                          accessPath: List[String],
                          reasonAccessPath: List[String],
                          backendName: String) {

  override def toString = s"LocalizedError(error=${error.loggableMessage}, accessPath=${accessPath.map(_.hashCode.toHexString)}, reasonAccessPath=${reasonAccessPath.map(_.hashCode.toHexString)}, backendName=$backendName)"
}

/** An access path holds a List of Numbers
  *
  * */
class AccessPath(val accessPath: List[Number]) {

  override def toString = s"AccessPath(accessPath=${accessPath.map(_.hashCode.toHexString)})"
}

case class ViperAst(p: Program, m: Hashable)
