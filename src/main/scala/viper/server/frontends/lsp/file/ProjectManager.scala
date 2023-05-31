// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2023 ETH Zurich.

package viper.server.frontends.lsp.file

// import scala.collection.mutable.HashSet
import viper.server.frontends.lsp.SetupProjectParams
import scala.collection.mutable.ArrayBuffer
import org.eclipse.lsp4j.Range

case class LeafInfo(private val roots: ArrayBuffer[String]) {
  def lastRoot: String = roots.head
  def addRoot(root: String) = {
    roots -= root
    roots += root
  }
  def removeRoot(root: String): Boolean = {
    roots -= root
    roots.isEmpty
  }
  def rootsIter: Iterator[String] = roots.iterator
}
object LeafInfo {
  def apply(root: String): LeafInfo = new LeafInfo(ArrayBuffer(root))
}

trait ProjectManager[A <: ProjectManager[A]] extends Manager[A] { this: A =>
  // val manager: FileManager

  var project: Either[Map[String, LeafManager], LeafInfo] = Left(Map())
  def removeFromProject(root: String) = {
    if (project.map(li => li.removeRoot(root)).getOrElse(false)) {
      project = Left(Map())
    }
  }
  def addToProject(root: String, getContents: Boolean): (Option[String], Option[Set[String]]) = {
    val oldProject = project match {
      case Left(p) => {
        project = Right(LeafInfo(root))
        Some(p.keySet)
      }
      case Right(li) => {
        li.addRoot(root)
        None
      }
    }
    val contents = if (getContents) Some(content.fileContent.mkString("\n")) else None
    (contents, oldProject)
  }

  def projectRoot: Option[String] = project.toOption.map(_.lastRoot)
  def projectLeaves: Option[Set[String]] = project.left.toOption.map(_.keySet)
  def isRoot: Boolean = project.isLeft

  def getInProject(uri: String): Manager[_] = if (uri == file_uri) this else project.left.toOption.get(uri)

  // def root: FileManager = if (isLeaf) {
  //   coordinator.getFile(project.toSeq(0))
  // } else manager
  // def isLeaf: Boolean = project.size == 1 && !project(file_uri)
  // def getProject: Seq[FileManager] = 

  // def toUnitProject(): Unit = {
  //   for (p <- project) {

  //   }
  // }

  // def removeFromProject(root: ProjectManager) = {
  //   project match {
  //     case Right(currProject) if currProject eq root => project = Left(HashMap())
  //     case _ => () // Shouldn't happen
  //   }
  // }
  // def removeFromProject(leaf: String) = {
  //   project match {
  //     case Left(project) => project.remove(leaf)
  //     case _ => () // Shouldn't happen
  //   }
  // }

  // def addToProject(root: FileManager) = {
  //   // Kill Project
  //   project match {
  //     case Left(currProject) if currProject.size > 0 =>
  //       coordinator.setupProject(file_uri, Array())
  //     case Right(value) => 
  //     case _ => ()
  //   }
  //   project = Right(root)
  // }

  def setupProject(newProject: Set[String]) = {
    val oldProject = project.left.toOption.getOrElse(Map())
    val toRemove = oldProject.keySet.diff(newProject)
    for (p <- toRemove) {
      coordinator.removeFromProject(p, file_uri)
    }
    val np = newProject.map(uri => uri -> coordinator.addToProject(uri, file_uri, !oldProject.contains(uri))).toMap
    for ((root, (_, leavesOpt)) <- np; leaves <- leavesOpt; leaf <- leaves) {
      coordinator.removeFromProject(leaf, root)
    }

    def getLeafManager(uri: String): LeafManager = oldProject.getOrElse(uri, LeafManager(file_uri, np(uri)._1.get, coordinator))
    this.project = Left(newProject.map(uri => uri -> getLeafManager(uri)).toMap)

    val setupProject = SetupProjectParams(file_uri, newProject.toArray)
    coordinator.client.requestSetupProject(setupProject)
  }

  def handleChangeInLeaf(leaf: String, range: Range, text: String): Unit = {
    project match {
      case Left(project) => project.get(leaf) match {
        case None => coordinator.logger.error(s"handleChangeInLeaf called on project without leaf (${leaf})")
        case Some(v) => v.handleChange(range, text)
      }
      case _ => coordinator.logger.error("handleChangeInLeaf called on non-root")
    }
  }

  override def handleChange(range: Range, text: String): Unit = {
    super.handleChange(range, text)
    project.foreach(_.rootsIter.foreach(root => {
      coordinator.handleChangeInLeaf(root, file_uri, range, text)
    }))
  }
}
