package viper.server.frontends.mcp

import akka.Done
import io.modelcontextprotocol.json.{McpJsonDefaults, TypeRef}
import io.modelcontextprotocol.server.transport.StdioServerTransportProvider
import io.modelcontextprotocol.server.{McpAsyncServer, McpServer}
import io.modelcontextprotocol.spec.{McpServerSession, McpServerTransport}
import io.modelcontextprotocol.spec.McpSchema.{JSONRPCMessage, ServerCapabilities}
import reactor.core.publisher.Mono
import viper.server.ViperConfig
import viper.server.utility.Helpers.validateViperFile
import viper.server.core.{VerificationExecutionContext, ViperBackendConfig, ViperCoreServer, ViperCoreServerUtils}
import viper.server.vsi.DefaultVerificationServerStart
import viper.silver.reporter.{OverallFailureMessage, OverallSuccessMessage, AstConstructionFailureMessage, ExceptionReport}

import java.time.{Duration => JDuration}
import java.util.concurrent.atomic.AtomicBoolean
import java.util.{List => JList}
import scala.concurrent.{Future, Promise}
import scala.util.control.NonFatal

class ViperMcpServer(config: ViperConfig)(override implicit val executor: VerificationExecutionContext)
  extends ViperCoreServer(config)(executor) with DefaultVerificationServerStart {

  private var mcpServer: McpAsyncServer = _

  // MCP has no shutdown request: over stdio, the client ends the session by closing stdin.
  // The SDK then closes the session's transport, so we wrap each transport to get notified.
  private val transport = new StdioServerTransportProvider(McpJsonDefaults.getMapper) {
    override def setSessionFactory(factory: McpServerSession.Factory): Unit = {
      // we pass a function where it expects a Factory interface, this function gets converted
      // to Factory by compiler since it's a "SAM type" (lambda gets converted to .create method)
      super.setSessionFactory((t: McpServerTransport) =>
        factory.create(new CloseNotifyingTransport(t, () => onSessionClosed())))
    }
  }

  private val stoppedPromise: Promise[Done] = Promise()
  private val isClosing = new AtomicBoolean(false)
  /** The returned future is completed when the MCP session has ended and the server has been stopped. */
  def stopped(): Future[Done] = stoppedPromise.future

  override def start(): Future[Done] = {
    // first make sure all logging goes to stderr/null (not stdout since MCP uses it for its protocol).
    McpLogging.redirectRootLoggerToNull()

    super.start() map { _ =>
      mcpServer = McpServer.async(transport)
        .serverInfo("viper-mcp-server", "0.1.0")
        .capabilities(ServerCapabilities.builder().tools(true).build())
        .tools(
          McpTools.verifyFileSpec(verifyFile),
          McpTools.verifySnippetSpec(verifyFile),
          McpTools.flushCacheSpec(() => flushCache())
        )
        .build()

      // fallback if the client terminates us (e.g. SIGTERM) instead of closing stdin
      sys.addShutdownHook {
        try mcpServer.closeGracefully().block(JDuration.ofSeconds(5))
        catch { case NonFatal(_) => }
      }

      Done
    }
  }

  def verifyFile(file: String, backend: ViperBackendConfig): Future[String] = {
    if (!validateViperFile(file)) return Future.successful("Invalid file path")
    val astId = requestAst(file, backend)
    val verId = verifyWithAstJob(file, astId, backend)
    if (astId.id < 0 || verId.id < 0) Future.successful("Server busy")
    else ViperCoreServerUtils.getMessagesFuture(this, verId).map(_.collect {
      case m: OverallSuccessMessage => s"Verified (${m.verificationTime}ms)"
      case m: OverallFailureMessage => ("Failed:" +: m.result.errors.map(_.readableMessage)).mkString("\n")
      case m: AstConstructionFailureMessage => ("Parse/typecheck failed:" +: m.result.errors.map(_.readableMessage)).mkString("\n")
      case m: ExceptionReport => s"Exception: ${m.e}"
    }.mkString("\n"))
  }

  /** Called (possibly several times, from SDK threads) when the stdio session ends. */
  private def onSessionClosed(): Unit = {
    if (isClosing.compareAndSet(false, true)) {
      try {
        stop().onComplete(_ => stoppedPromise.trySuccess(Done))
      } catch {
        case NonFatal(_) => stoppedPromise.trySuccess(Done)
      }
    }
  }

  /** Delegates everything to `inner` and calls `onClose` once the transport is closed. */
  private class CloseNotifyingTransport(inner: McpServerTransport, onClose: () => Unit) extends McpServerTransport {
    override def sendMessage(message: JSONRPCMessage): Mono[Void] = inner.sendMessage(message)
    override def unmarshalFrom[T](data: Object, typeRef: TypeRef[T]): T = inner.unmarshalFrom(data, typeRef)
    override def protocolVersions(): JList[String] = inner.protocolVersions()
    override def close(): Unit = {
      inner.close()
      onClose()
    }
    override def closeGracefully(): Mono[Void] = inner.closeGracefully().doFinally(_ => onClose())
  }
}
