// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2020 ETH Zurich.

package viper.server.core

import scala.language.postfixOps
import ch.qos.logback.classic.Logger
import viper.server.core.ViperCache.logger
import viper.server.vsi._
import viper.silver.ast.{Add, And, AnonymousDomainAxiom, AnySetCardinality, AnySetContains, AnySetIntersection, AnySetMinus, AnySetSubset, AnySetUnion, Apply, Applying, Assert, Cached, CondExp, ConsInfo, CurrentPerm, Div, Domain, DomainFunc, DomainFuncApp, EmptyMultiset, EmptySeq, EmptySet, EpsilonPerm, EqCmp, Exhale, Exists, ExplicitMultiset, ExplicitSeq, ExplicitSet, FalseLit, Field, FieldAccess, FieldAccessPredicate, FieldAssign, Fold, ForPerm, Forall, FractionalPerm, FullPerm, FuncApp, Function, GeCmp, Goto, GtCmp, Hashable, If, Implies, Inhale, InhaleExhaleExp, IntLit, IntPermMul, Label, LabelledOld, LeCmp, Let, LocalVar, LocalVarAssign, LocalVarDecl, LocalVarDeclStmt, LtCmp, MagicWand, Method, MethodCall, Minus, Mod, Mul, NamedDomainAxiom, NeCmp, NewStmt, NoPerm, Node, Not, NullLit, Old, Or, Package, PermAdd, PermDiv, PermGeCmp, PermGtCmp, PermLeCmp, PermLtCmp, PermMinus, PermMul, PermSub, Position, Predicate, PredicateAccess, PredicateAccessPredicate, Program, RangeSeq, SeqAppend, SeqContains, SeqDrop, SeqIndex, SeqLength, SeqTake, SeqUpdate, Seqn, Sub, Trigger, TrueLit, Unfold, Unfolding, While, WildcardPerm}
import viper.silver.utility.CacheHelper
import viper.silver.verifier.errors._
import viper.silver.verifier.{AbstractVerificationError, VerificationError, errors}

import scala.annotation.tailrec
import scala.collection.mutable.{Map => MutableMap}

// ===== CACHE OBJECT ==================================================================

object ViperCache extends Cache {

  private var _backendSpecificCache: Boolean = false

  var _logger: Logger = _
  def logger: Logger = _logger

  def initialize(logger: Logger, backendSpecificCache: Boolean): Unit = {
    _backendSpecificCache = backendSpecificCache
    _logger = logger
    resetCache()
  }

  def applyCache(
        backendName: String,
        file: String,
        p: Program): (Program, List[CacheResult]) = {

    val file_key = getKey(backendName, file)
    val cacheable_ast = ViperAst(p)
    val (output_ast, cache_entries) = super.retrieve(file_key, cacheable_ast)
    val output_prog = output_ast.asInstanceOf[ViperAst].p

    val ver_results = cache_entries.map(ce => {
      val concerning_method: Method = ce.concerning.asInstanceOf[ViperMethod].m
      val content = ce.content.asInstanceOf[ViperCacheContent]
      val ver_errors = updateErrorLocation(file_key, p, concerning_method, content)
      CacheResult(concerning_method, ver_errors)
    })

    (output_prog, ver_results)
  }

  private def updateErrorLocation(
                file_key: String,
                p: Program,
                m: Method,
                cacheContent: ViperCacheContent): List[VerificationError] = {

    cacheContent.errors.map(e => updateErrorLocation(file_key, p, m, e))
  }

  private def updateErrorLocation(
                file_key: String,
                p: Program,
                m: Method,
                error: LocalizedError): VerificationError = {

    assert(error.error != null && error.accessPath != null && error.reasonAccessPath != null)

    //get the corresponding offending node in the new AST
    //TODO: are these casts ok?
    val offendingNode = ViperCacheHelper.getNode(file_key, p, error.accessPath, error.error.offendingNode).asInstanceOf[Option[errors.ErrorNode]]
    val reasonOffendingNode = ViperCacheHelper.getNode(file_key, p, error.reasonAccessPath, error.error.reason.offendingNode).asInstanceOf[Option[errors.ErrorNode]]

    if (offendingNode.isEmpty || reasonOffendingNode.isEmpty) {
      throw new Exception(s"Cache error: no corresponding node found for error: $error")
    }

    //create a new VerificationError that only differs in the Position of the offending Node
    //the cast is fine, because the offending Nodes are supposed to be ErrorNodes
    val updatedOffendingNode = updatePosition(error.error.offendingNode, offendingNode.get.pos).asInstanceOf[errors.ErrorNode]
    val updatedReasonOffendingNode = updatePosition(error.error.reason.offendingNode, reasonOffendingNode.get.pos).asInstanceOf[errors.ErrorNode]
    //TODO: how to also update the position of error.error.reason.offendingNode?
    val updatedError = error.error.withNode(updatedOffendingNode).asInstanceOf[AbstractVerificationError]
    setCached(updatedError)
  }

  def setCached(error: AbstractVerificationError): AbstractVerificationError = {
    error match {
      case e: Internal => e.copy(cached = true)
      case e: AssignmentFailed => e.copy(cached = true)
      case e: CallFailed => e.copy(cached = true)
      case e: ContractNotWellformed => e.copy(cached = true)
      case e: PreconditionInCallFalse => e.copy(cached = true)
      case e: PreconditionInAppFalse => e.copy(cached = true)
      case e: ExhaleFailed => e.copy(cached = true)
      case e: InhaleFailed => e.copy(cached = true)
      case e: IfFailed => e.copy(cached = true)
      case e: WhileFailed => e.copy(cached = true)
      case e: AssertFailed => e.copy(cached = true)
      case e: TerminationFailed => e.copy(cached = true)
      case e: PostconditionViolated => e.copy(cached = true)
      case e: FoldFailed => e.copy(cached = true)
      case e: UnfoldFailed => e.copy(cached = true)
      case e: PackageFailed => e.copy(cached = true)
      case e: ApplyFailed => e.copy(cached = true)
      case e: LoopInvariantNotPreserved => e.copy(cached = true)
      case e: LoopInvariantNotEstablished => e.copy(cached = true)
      case e: FunctionNotWellformed => e.copy(cached = true)
      case e: PredicateNotWellformed => e.copy(cached = true)
      case e: MagicWandNotWellformed => e.copy(cached = true)
      case e: LetWandFailed => e.copy(cached = true)
      case e: HeuristicsFailed => e.copy(cached = true)
      case e: VerificationErrorWithCounterexample => e.copy(cached = true)
      case e: AbstractVerificationError =>
        logger.warn("Setting a verification error to cached was not possible for " + e + ". Make sure to handle this types of errors")
        e
    }
  }

  def updatePosition(n: Node, pos: Position): Node = {
    n match {
      case t: Trigger => t.copy()(pos, t.info, t.errT)
      case t: Program => t.copy()(pos, t.info, t.errT)

      //Members
      case t: Field => t.copy()(pos, t.info, t.errT)
      case t: Function => t.copy()(pos, t.info, t.errT)
      case t: Method => t.copy()(pos, t.info, t.errT)
      case t: Predicate => t.copy()(pos, t.info, t.errT)
      case t: Domain => t.copy()(pos, t.info, t.errT)

      //DomainMembers
      case t: NamedDomainAxiom => t.copy()(pos, t.info, t.domainName, t.errT)
      case t: AnonymousDomainAxiom => t.copy()(pos, t.info, t.domainName, t.errT)
      case t: DomainFunc => t.copy()(pos, t.info, t.domainName, t.errT)

      //Statements
      case t: NewStmt => t.copy()(pos, t.info, t.errT)
      case t: LocalVarAssign => t.copy()(pos, t.info, t.errT)
      case t: FieldAssign => t.copy()(pos, t.info, t.errT)
      case t: Fold => t.copy()(pos, t.info, t.errT)
      case t: Unfold => t.copy()(pos, t.info, t.errT)
      case t: Package => t.copy()(pos, t.info, t.errT)
      case t: Apply => t.copy()(pos, t.info, t.errT)
      case t: Inhale => t.copy()(pos, t.info, t.errT)
      case t: Exhale => t.copy()(pos, t.info, t.errT)
      case t: Assert => t.copy()(pos, t.info, t.errT)
      case t: MethodCall => t.copy()(pos, t.info, t.errT)
      case t: Seqn => t.copy()(pos, t.info, t.errT)
      case t: While => t.copy()(pos, t.info, t.errT)
      case t: If => t.copy()(pos, t.info, t.errT)
      case t: Label => t.copy()(pos, t.info, t.errT)
      case t: Goto => t.copy()(pos, t.info, t.errT)
      case t: LocalVarDeclStmt => t.copy()(pos, t.info, t.errT)

      case t: LocalVarDecl => t.copy()(pos, t.info, t.errT)

      //Expressions
      case t: FalseLit => t.copy()(pos, t.info, t.errT)
      case t: NullLit => t.copy()(pos, t.info, t.errT)
      case t: TrueLit => t.copy()(pos, t.info, t.errT)
      case t: IntLit => t.copy()(pos, t.info, t.errT)
      case t: LocalVar => t.copy(typ = t.typ)(pos, t.info, t.errT)
      case t: viper.silver.ast.Result => t.copy(t.typ)(pos, t.info, t.errT)
      case t: FieldAccess => t.copy()(pos, t.info, t.errT)
      case t: PredicateAccess => t.copy()(pos, t.info, t.errT)
      case t: Unfolding => t.copy()(pos, t.info, t.errT)
      case t: Applying => t.copy()(pos, t.info, t.errT)
      case t: CondExp => t.copy()(pos, t.info, t.errT)
      case t: Let => t.copy()(pos, t.info, t.errT)
      case t: Exists => t.copy()(pos, t.info, t.errT)
      case t: Forall => t.copy()(pos, t.info, t.errT)
      case t: ForPerm => t.copy()(pos, t.info, t.errT)
      case t: InhaleExhaleExp => t.copy()(pos, t.info, t.errT)
      case t: WildcardPerm => t.copy()(pos, t.info, t.errT)
      case t: FullPerm => t.copy()(pos, t.info, t.errT)
      case t: NoPerm => t.copy()(pos, t.info, t.errT)
      case t: EpsilonPerm => t.copy()(pos, t.info, t.errT)
      case t: CurrentPerm => t.copy()(pos, t.info, t.errT)
      case t: FieldAccessPredicate => t.copy()(pos, t.info, t.errT)
      case t: PredicateAccessPredicate => t.copy()(pos, t.info, t.errT)

      //Binary operators
      case t: Add => t.copy()(pos, t.info, t.errT)
      case t: Sub => t.copy()(pos, t.info, t.errT)
      case t: Mul => t.copy()(pos, t.info, t.errT)
      case t: Div => t.copy()(pos, t.info, t.errT)
      case t: Mod => t.copy()(pos, t.info, t.errT)
      case t: LtCmp => t.copy()(pos, t.info, t.errT)
      case t: LeCmp => t.copy()(pos, t.info, t.errT)
      case t: GtCmp => t.copy()(pos, t.info, t.errT)
      case t: GeCmp => t.copy()(pos, t.info, t.errT)
      case t: EqCmp => t.copy()(pos, t.info, t.errT)
      case t: NeCmp => t.copy()(pos, t.info, t.errT)
      case t: Or => t.copy()(pos, t.info, t.errT)
      case t: And => t.copy()(pos, t.info, t.errT)
      case t: Implies => t.copy()(pos, t.info, t.errT)
      case t: MagicWand => t.copy()(pos, t.info, t.errT)
      case t: FractionalPerm => t.copy()(pos, t.info, t.errT)
      case t: PermDiv => t.copy()(pos, t.info, t.errT)
      case t: PermAdd => t.copy()(pos, t.info, t.errT)
      case t: PermSub => t.copy()(pos, t.info, t.errT)
      case t: PermMul => t.copy()(pos, t.info, t.errT)
      case t: IntPermMul => t.copy()(pos, t.info, t.errT)
      case t: PermLtCmp => t.copy()(pos, t.info, t.errT)
      case t: PermLeCmp => t.copy()(pos, t.info, t.errT)
      case t: PermGtCmp => t.copy()(pos, t.info, t.errT)
      case t: PermGeCmp => t.copy()(pos, t.info, t.errT)
      case t: AnySetUnion => t.copy()(pos, t.info, t.errT)
      case t: AnySetIntersection => t.copy()(pos, t.info, t.errT)
      case t: AnySetSubset => t.copy()(pos, t.info, t.errT)
      case t: AnySetMinus => t.copy()(pos, t.info, t.errT)
      case t: AnySetContains => t.copy()(pos, t.info, t.errT)

      //Unary operators
      case t: Minus => t.copy()(pos, t.info, t.errT)
      case t: Not => t.copy()(pos, t.info, t.errT)
      case t: PermMinus => t.copy()(pos, t.info, t.errT)
      case t: Old => t.copy()(pos, t.info, t.errT)
      case t: LabelledOld => t.copy()(pos, t.info, t.errT)
      case t: AnySetCardinality => t.copy()(pos, t.info, t.errT)
      case t: FuncApp => t.copy()(pos, t.info, t.typ, t.errT)
      case t: DomainFuncApp => t.copy()(pos, t.info, t.typ, t.domainName, t.errT)
      case t: EmptySeq => t.copy()(pos, t.info, t.errT)
      case t: ExplicitSeq => t.copy()(pos, t.info, t.errT)
      case t: RangeSeq => t.copy()(pos, t.info, t.errT)
      case t: SeqAppend => t.copy()(pos, t.info, t.errT)
      case t: SeqIndex => t.copy()(pos, t.info, t.errT)
      case t: SeqTake => t.copy()(pos, t.info, t.errT)
      case t: SeqDrop => t.copy()(pos, t.info, t.errT)
      case t: SeqContains => t.copy()(pos, t.info, t.errT)
      case t: SeqUpdate => t.copy()(pos, t.info, t.errT)
      case t: SeqLength => t.copy()(pos, t.info, t.errT)

      //others
      case t: EmptySet => t.copy()(pos, t.info, t.errT)
      case t: ExplicitSet => t.copy()(pos, t.info, t.errT)
      case t: EmptyMultiset => t.copy()(pos, t.info, t.errT)
      case t: ExplicitMultiset => t.copy()(pos, t.info, t.errT)
      case t =>
        logger.warn("The location was not updated for the node " + t + ". Make sure to handle this type of node")
        t
    }
  }

  def update(
        backendName: String,
        file: String,
        method: Method,
        program: Program,
        errors: List[AbstractVerificationError]): List[CacheEntry] = {

    val viperMethod = ViperMethod(method)
//    val deps: List[Concerning] = program.getDependencies(program, method).map(h => ViperMember(h))
    val deps: List[Member] = viperMethod.getDependencies(ViperAst(program))
    val content = createCacheContent(backendName, file, program, method, errors)
    val file_key = getKey(backendName, file)
    super.update(file_key, ViperMethod(method), deps, content)
  }

  def forgetFile(backendName: String, file: String): Option[String] = {
    val key = getKey(backendName, file)
    super.forgetFile(key)
  }

  override def resetCache(): Unit = {
    ViperCacheHelper.node_hash_memo.clear()
    _cache.clear()
  }

  def getKey(file: String, backendName: String): String = {
    (if (_backendSpecificCache) backendName else "") + file
  }

  def createCacheContent(
        backendName: String, file: String,
        p: Program, m: Method,
        errors: List[AbstractVerificationError]): ViperCacheContent = {

    implicit val key: String = getKey(backendName, file)
    val loc_errs = errors.map(err =>
      LocalizedError(err,
        ViperCacheHelper.getAccessPath(err.offendingNode, p),
        ViperCacheHelper.getAccessPath(err.reason.offendingNode, p),
        backendName))
    ViperCacheContent(loc_errs)
  }
}


object ViperCacheHelper {
  private val _node_hash_memo = MutableMap.empty[String, MutableMap[Node, String]]
  def node_hash_memo: MutableMap[String, MutableMap[Node, String]] = _node_hash_memo

  protected def hex(h: String): String = h.hashCode.toHexString

  /**
    * This method is used for computing unique-ish hashes of AST nodes. `idx` represents
    * the node's index as child of the parent node. `idx` should be included in the hash
    * calculations to avoid having the same hash for two children of the same parent.
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
    *
    *  @see [[forgetFile]].
    */
  @tailrec
  private def getHashForNode(node: Node, idx: Int)(implicit key: String): String = {
    def addIdxToHash(hash: String): String = idx.toString + hash

    node match {
      case m: Method => addIdxToHash(removeBody(m).entityHash)
      case hn: Hashable => addIdxToHash(hn.entityHash)
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
              val hash = addIdxToHash(CacheHelper.computeEntityHash("", node))
              _node_hash_memo(key)(n) = hash
              hash
          }
          case None =>
            _node_hash_memo(key) = MutableMap.empty[Node, String]
            getHashForNode(n, idx)
        }
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
      curr.subnodes.zipWithIndex.foreach { case (node, idx) =>
        // Go through all the node's children and recursively compute the path to the sought node from there
        computeAccessPath(nodeToFind, node) match {
          case Some(access_path) =>
            // If a path is returned, the right subnodes was found. Compute hash of current node
            // and append it to the path list.
            val hash = getHashForNode(node, idx)
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

  private def str(n: Node, idx: Int)(implicit key: String) = s"(${n.toOneLinerStr()} -> ${getHashForNode(n, idx).hashCode.toHexString})"

  /** Finds a node in a program by traversing the provided accessPath
    * */
  def getNode(
        implicit file_key: String,
        p: Program,
        accessPath: List[String],
        oldNode: Node): Option[Node] = {

    logger.trace(s"looking for last node on access path ${accessPath.map(ViperCacheHelper.hex)}...")

    // start at root and traverse path node (hash) by node (hash)
    // the second element indicates the node's index in the list of children of its parent
    var curr: (Node, Int) = (p, 0) // the root is by definition at index 0
    accessPath.foreach(hash => {
      logger.trace(s" ... curr = ${str(curr._1, curr._2)}")
      logger.trace(s" ... considering hash ${hex(hash)} among subnodes ${curr._1.subnodes.zipWithIndex.map{ case (subnode, subIdx) => str(subnode, subIdx)}}...")

      // In the list of the current node's children, find the one who's hash matches the hash
      // specified by the accesspath.
      curr._1.subnodes.zipWithIndex.find { case (sub, subIdx) => getHashForNode(sub, subIdx) == hash } match {
        case Some(hashed_subnode) =>
          // hash corresponds to a subnode of curr.
          curr = hashed_subnode
        case None =>
          // no subnode of curr corresponds to the hash
          return None
      }
    })

    // If path traversal successful check that found node and old node's classes match
    if (curr._1.getClass == oldNode.getClass) {
      logger.trace(s" ==> found node: (${curr._1.toOneLinerStr()} -> ${getHashForNode(curr._1, curr._2).hashCode.toHexString})")
      Some(curr._1)
    } else {
      logger.trace(s" ==> node not found!")
      None
    }
  }

  def removeBody(m: Method): Method = m.copy(body = None)(m.pos, ConsInfo(m.info, Cached), m.errT)
}

// ===== AUXILIARY CLASSES ==================================================================

case class ViperCacheContent(errors: List[LocalizedError]) extends CacheContent

/** A localized error contains the Abstract Verification Error, paths
  *
  * */
case class LocalizedError(
              error: AbstractVerificationError,
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

case class ViperAst(p: Program) extends Ast {

  override def compose(cs: List[CacheableMember]): Ast = {
    // FIXME Use polymorphic types instead of casts!
    val new_methods: List[Method] = cs filter { _.isInstanceOf[ViperMethod] } map { vm => vm.asInstanceOf[ViperMethod].m }
    val new_predicates: List[Predicate] = cs filter { _.isInstanceOf[ViperPredicate] } map { vp => vp.asInstanceOf[ViperPredicate].p }
    val new_functions: List[Function] = cs filter { _.isInstanceOf[ViperFunction] } map { vf => vf.asInstanceOf[ViperFunction].f }
    val new_program = Program(p.domains, p.fields,
      new_functions, new_predicates, new_methods, p.extensions)(p.pos, p.info, p.errT)
    ViperAst(new_program)
  }

  override def decompose(): List[CacheableMember] = {
    p.methods.map(m => ViperMethod(m)) ++
      p.predicates.map(p => ViperPredicate(p)) ++
      p.functions.map(f => ViperFunction(f)) toList
  }

  override def equals(other: Ast): Boolean = {
    this.toString == other.toString
  }
}

case class CacheResult(method: Method, verification_errors: List[VerificationError])

case class ViperMember(h: Hashable) extends Member {

  def hash(): String = {
    h.entityHash
  }
}

case class ViperMethod(m: Method) extends CacheableMember {

  def hash(): String = {
    m.entityHash
  }

  def transform: CacheableMember = {
    ViperMethod(m.copy(body = None)(m.pos, ConsInfo(m.info, Cached), m.errT))
  }

  def getDependencies(ast: Ast): List[Member] = {
    val p = ast.asInstanceOf[ViperAst].p
    p.getDependencies(p, m).map(h => ViperMember(h))
  }
}

case class ViperPredicate(p: Predicate) extends CacheableMember {
  def hash(): String = {
    p.entityHash
  }

  def transform: CacheableMember = {
    ViperPredicate(p.copy()(p.pos, ConsInfo(p.info, Cached), p.errT))
  }

  def getDependencies(ast: Ast): List[Member] = {
    val p = ast.asInstanceOf[ViperAst].p
    p.members filter (_.entityHash != hash()) map (h => ViperMember(h)) toList
  }
}

case class ViperFunction(f: Function) extends CacheableMember {
  def hash(): String = {
    f.entityHash
  }

  def transform: CacheableMember = {
    ViperFunction(f.copy()(f.pos, ConsInfo(f.info, Cached), f.errT))
  }

  def getDependencies(ast: Ast): List[Member] = {
    val p = ast.asInstanceOf[ViperAst].p
    p.members filter (_.entityHash != hash()) map (h => ViperMember(h)) toList
  }
}