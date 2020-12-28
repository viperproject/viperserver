package viper.server.core


import ch.qos.logback.classic.Logger
import viper.server.utility.AstGenerator
import viper.server.vsi.AstConstructionException
import viper.silver.ast.Program
import viper.silver.reporter.ExceptionReport


object ViperFileNotFoundException extends AstConstructionException
object AstConstructionInterrupted extends AstConstructionException
object InvalidArgumentsException extends AstConstructionException
object AstConstructionFailureException extends AstConstructionException
object OutOfResourcesException extends AstConstructionException

case class ServerCrashException(e: Throwable) extends Exception(e)


import scala.concurrent.{ExecutionContext, Future, Promise}

class AstWorker(val arg_list: List[String],
                val logger: Logger)(implicit val ec: ExecutionContext) extends MessageReportingTask {

  private val _artifact_pro: Promise[Program] = Promise()
  override def artifact: Option[Future[Program]] = Some(_artifact_pro.future)

  private def constructAst(): Future[Program] = Future {

    //println(">>> AstWorker.constructAst()")

    val file: String = arg_list.last

    val reporter = new ActorReporter("AstGenerationReporter")
    val astGen = new AstGenerator(logger, reporter)

    val ast_option: Option[Program] = try {
      astGen.generateViperAst(file)
    } catch {
      case _: java.nio.file.NoSuchFileException =>
        println("The file for which verification has been requested was not found.")
        registerTaskEnd(false)
        throw ViperFileNotFoundException
      case e: InterruptedException =>
        logger.info(s"AstWorker has been interrupted: $e")
        registerTaskEnd(false)
        throw AstConstructionInterrupted
      case e: java.nio.channels.ClosedByInterruptException =>
        logger.info(s"AstWorker has been interrupted: $e")
        registerTaskEnd(false)
        throw AstConstructionInterrupted
      case e: Throwable =>
        reporter report ExceptionReport(e)
        logger.trace(s"Creation/Execution of an AstGenerator instance resulted in an exception.", e)
        registerTaskEnd(false)
        throw ServerCrashException(e)
    }

    ast_option match {
      case Some(ast) =>
        registerTaskEnd(true)
        ast
      case None =>
        logger.info("The file for which verification has been requested contained syntax errors, type errors, " +
          "or is simply inconsistent.")
        registerTaskEnd(false)
        throw AstConstructionFailureException
    }
  }

  override def run(): Unit = {
    //println(">>> AstWorker.run()")
    _artifact_pro.completeWith(constructAst())
  }

}
