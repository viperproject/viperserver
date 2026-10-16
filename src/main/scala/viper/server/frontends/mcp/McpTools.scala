package viper.server.frontends.mcp

import io.modelcontextprotocol.server.McpServerFeatures.AsyncToolSpecification
import io.modelcontextprotocol.spec.McpSchema.{CallToolResult, Tool}
import reactor.core.publisher.Mono
import viper.server.core.{VerificationExecutionContext, ViperBackendConfig}

import java.nio.file.Files
import java.util.{List => JList, Map => JMap}
import scala.jdk.FutureConverters._
import scala.util.control.NonFatal
import scala.concurrent.Future

object McpTools {

  /* helper methods -------------------------------------------------------------------------------------*/
  private def toolResult(msg: String): CallToolResult =
    CallToolResult.builder().addTextContent(msg).build()

  private def errorResult(msg: String): CallToolResult =
    CallToolResult.builder().addTextContent(msg).isError(true).build()

  private def getBackendFromArgs(args: JMap[String, Object]): ViperBackendConfig = {
    val backendName = args.getOrDefault("backend", "silicon").toString
    val customArgs = args.getOrDefault("custom_args", "").toString
    ViperBackendConfig(backendName, customArgs)
  }


  /* verify_file tool -----------------------------------------------------------------------------------*/
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

  def verifyFileSpec(verifyFileCallback: (String, ViperBackendConfig) => Future[String])
                    (implicit executor: VerificationExecutionContext): AsyncToolSpecification = AsyncToolSpecification.builder()
    .tool(verifyFileTool)
    .callHandler((_, request) => {
      // get and validate tool parameters
      val path = request.arguments().get("path")
      val backend = getBackendFromArgs(request.arguments())

      if (path == null) {
        Mono.just(errorResult("Missing required parameter: `path`"))
      }
      else {
        Mono.fromCompletionStage(() =>
          verifyFileCallback(path.toString, backend)
            .map(toolResult)
            .recover { case NonFatal(e) => errorResult(s"Error: $e") }
            .asJava
        )
      }
    })
    .build()

  /* verify_snippet tool -----------------------------------------------------------------------------------*/
  private val verifySnippetSchema = JMap.of(
    "type", "object",
    "properties", JMap.of(
      "code", JMap.of("type", "string", "description", "Viper code snippet to verify"),
      "backend", JMap.of("type", "string", "description", "Name of backend to use for verification (silicon|carbon|custom)"),
      "custom_args", JMap.of("type", "string", "description", "Extra arguments to pass to the backend verifier"),
    ),
    "required", JList.of("code")
  )
  private val verifySnippetTool: Tool = Tool.builder("verify_snippet", verifySnippetSchema)
    .description("Formally verify a Viper code snippet")
    .build()

  def verifySnippetSpec(verifyFileCallback: (String, ViperBackendConfig) => Future[String])
                       (implicit executor: VerificationExecutionContext): AsyncToolSpecification = AsyncToolSpecification.builder()
    .tool(verifySnippetTool)
    .callHandler((_, request) => {
      // get and validate tool parameters
      val code = request.arguments().get("code")
      val backend = getBackendFromArgs(request.arguments())

      if (code == null) {
        Mono.just(errorResult("Missing required parameter: `code`"))
      }
      else {
        val tempPath = Files.createTempFile("viper-snippet-", ".vpr")
        Files.writeString(tempPath, code.toString)
        Mono.fromCompletionStage(() =>
          verifyFileCallback(tempPath.toString, backend)
            .map(toolResult)
            .recover { case NonFatal(e) => errorResult(s"Error: $e") }
            .andThen{ case _ => Files.deleteIfExists(tempPath) }
            .asJava
        )
      }
    })
    .build()

  /* flush_cache tool -----------------------------------------------------------------------------------*/
  private val flushCacheSchema: JMap[String, Object] = JMap.of("type", "object")
  private val flushCacheTool: Tool = Tool.builder("flush_cache", flushCacheSchema)
    .description("Flush the full verification cache")
    .build()

  def flushCacheSpec(flushCacheCallback: () => Boolean): AsyncToolSpecification = AsyncToolSpecification.builder()
    .tool(flushCacheTool)
    .callHandler((_, _) => {
      flushCacheCallback()
      Mono.just(toolResult("{\"flushed\": true}"))
    })
    .build()
}
