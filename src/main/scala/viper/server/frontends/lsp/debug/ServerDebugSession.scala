// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2025 ETH Zurich.

package viper.server.frontends.lsp.debug

import viper.server.core.VerificationExecutionContext
import viper.silicon.Silicon
import viper.silicon.debugger.SiliconDebugSession

import scala.concurrent.{Future, Promise}
import scala.util.Try

/**
  * A debug session as seen by ViperServer: the Silicon instance that produced it, the underlying
  * [[SiliconDebugSession]], and the information needed to relate it back to the file and the diagnostic the
  * user asked to debug.
  *
  * All operations on the underlying session are serialized onto a single chain of futures, because a session
  * owns a prover process and a mutable symbolic state and is therefore not thread-safe. They also must not run
  * on the LSP handler's thread: a prover round-trip can take arbitrarily long, and the thread pool that serves
  * LSP requests is the same one that runs verifications.
  */
class ServerDebugSession(val sessionId: String,
                         val uri: String,
                         /** The Silicon instance of a finished debug run, which the session stops when it is
                           * closed; None for the session of a paused verification, which goes on afterwards. */
                         val silicon: Option[Silicon],
                         private val inner: SiliconDebugSession)
                        (implicit executor: VerificationExecutionContext) {

  private var queue: Future[Unit] = Future.unit
  private var closed: Boolean = false

  /** Runs `op` on the underlying session once all previously submitted operations have finished. */
  def run[T](op: SiliconDebugSession => T): Future[T] = synchronized {
    if (closed) {
      Future.failed(new IllegalStateException(s"The debug session $sessionId has been closed."))
    } else {
      val p = Promise[T]()
      queue = queue.transformWith(_ => Future {
        p.complete(Try(op(inner)))
        ()
      }(executor))(executor)
      p.future
    }
  }

  def isClosed: Boolean = synchronized(closed)

  private var closeListeners: Seq[String => Unit] = Seq.empty

  /** Registers a callback that is invoked with the reason once this session is closed, e.g. because the file
    * was edited. The debug adapter uses it to end the DAP session that shows this debug session. */
  def onClose(listener: String => Unit): Unit = synchronized { closeListeners :+= listener }

  /** Stops the prover(s) of this session and releases the symbolic state it kept alive. */
  def close(reason: String): Future[Unit] = synchronized {
    if (closed) {
      Future.unit
    } else {
      closed = true
      closeListeners.foreach(l => Try(l(reason)))
      queue.transformWith(_ => Future {
        Try(inner.close())
        silicon.foreach(s => Try(s.stop()))
        ()
      }(executor))(executor)
    }
  }
}
