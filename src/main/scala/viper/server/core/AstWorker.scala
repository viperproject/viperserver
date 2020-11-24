package viper.server.core


import ch.qos.logback.classic.Logger
import viper.server.utility.AstGenerator
import viper.server.utility.Helpers.getArgListFromArgString
import viper.silver.ast.Program


abstract class AstConstructionException extends Exception
object ViperFileNotFoundException extends AstConstructionException
object InvalidArgumentsException extends AstConstructionException
object AstConstructionFailureException extends AstConstructionException
object OutOfResourcesException extends AstConstructionException

case class ServerCrashException(e: Throwable) extends Exception(e)


import scala.concurrent.{ExecutionContext, Future, Promise}

class AstWorker(val input: String,
                val logger: Logger)(implicit val ec: ExecutionContext) extends MessageReportingTask {

  //  private var _ast: Promise[Program] =
  private val _artifact_pro: Promise[Program] = Promise()
  override def artifact: Future[Program] = _artifact_pro.future

  private def constructAst(): Future[Program] = Future {

    println(">>> AstWorker.constructAst()")

    val arg_list = getArgListFromArgString(input)
    val file: String = arg_list.last

    val astGen = new AstGenerator(logger, new ActorReporter("AstGenerationReporter"))
    var ast_option: Option[Program] = None
    try {
      ast_option = astGen.generateViperAst(file)
    } catch {
      case _: java.nio.file.NoSuchFileException =>
        println("The file for which verification has been requested was not found.")
        throw ViperFileNotFoundException
    }
    val ast = ast_option match {
      case Some(a) =>
        a
      case _ =>
        println("The file for which verification has been requested contained syntax errors.")
        throw AstConstructionFailureException
    }
    ast
  }

  override def run(): Unit = {
    println(">>> AstWorker.run()")
    _artifact_pro.completeWith(constructAst())
  }

}
