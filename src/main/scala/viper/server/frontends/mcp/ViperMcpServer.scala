package viper.server.frontends.mcp

import akka.Done
import io.modelcontextprotocol.json.{McpJsonDefaults, TypeRef}
import io.modelcontextprotocol.server.transport.StdioServerTransportProvider
import io.modelcontextprotocol.server.{McpAsyncServer, McpServer}
import io.modelcontextprotocol.server.McpServerFeatures.AsyncToolSpecification
import io.modelcontextprotocol.spec.{McpServerSession, McpServerTransport}
import io.modelcontextprotocol.spec.McpSchema.{CallToolResult, JSONRPCMessage, ServerCapabilities, Tool}
import reactor.core.publisher.Mono
import viper.server.ViperConfig
import viper.server.utility.Helpers.validateViperFile
import viper.server.core.{VerificationExecutionContext, ViperBackendConfig, ViperCoreServer, ViperCoreServerUtils}
import viper.server.vsi.DefaultVerificationServerStart
import viper.silver.reporter.{OverallFailureMessage, OverallSuccessMessage, AstConstructionFailureMessage, ExceptionReport}

import java.time.{Duration => JDuration}
import java.util.concurrent.atomic.AtomicBoolean
import java.util.{List => JList, Map => JMap}
import scala.concurrent.{Future, Promise}
import scala.jdk.FutureConverters._
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


  private val verifyFileSchema = JMap.of(
    "type", "object",
    "properties", JMap.of(
      "path", JMap.of("type", "string", "description", "Absolute path for file to verify"),
      "backend", JMap.of("type", "string", "description", "Name of backend to use for verification (silicon|carbon|custom)"),
      "custom_args", JMap.of("type", "string", "description", "Extra arguments to pass to the backend verifier"),
    ),
    "required", JList.of("path")
  )
  private val verifyFileTool: Tool = Tool.builder("verify_file", verifyFileSchema)
    .description("Formally verify a Viper (.vpr) file")
    .build()

  override def start(): Future[Done] = {
    // first make sure all logging goes to stderr/null (not stdout since MCP uses it for its protocol).
    McpLogging.redirectRootLoggerToStdNull()

    super.start() flatMap { _ =>
      mcpServer = McpServer.async(transport)
        .serverInfo("viper-mcp-server", "0.1.0")
        .capabilities(ServerCapabilities.builder().tools(true).build())
        .build()

      // fallback if the client terminates us (e.g. SIGTERM) instead of closing stdin
      sys.addShutdownHook {
        try mcpServer.closeGracefully().block(JDuration.ofSeconds(5))
        catch { case NonFatal(_) => }
      }

      val verifyFileSpec = AsyncToolSpecification.builder()
        .tool(verifyFileTool)
        .callHandler((_, request) => {
          // get and validate tool parameters
          val path = request.arguments().get("path")
          val backendName = request.arguments().getOrDefault("backend", "silicon").toString
          val customArgs = request.arguments().getOrDefault("custom_args", "").toString
          val backend = ViperBackendConfig(backendName, customArgs)

          if (path == null) { Mono.just(errorResult("Missing required parameter: `path`")) }
          else {
            Mono.fromCompletionStage(() =>
              verifyFile(path.toString, backend)
                .map(res => CallToolResult.builder().addTextContent(res).build())
                .recover { case NonFatal(e) => errorResult(s"Error: $e") }
                .asJava
            )
          }
        })
        .build()

      // addTool is lazy (Mono): convert to a Future so start() completes once the tool is registered
      mcpServer.addTool(verifyFileSpec).toFuture.asScala.map(_ => Done)
    }
  }

  private def errorResult(msg: String): CallToolResult =
    CallToolResult.builder().addTextContent(msg).isError(true).build()

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
