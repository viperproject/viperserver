package viper.server.utility

import akka.stream.scaladsl.Source
import scala.concurrent.{Future, Promise}

/**
  * Taken from http://loicdescotte.github.io/posts/play-akka-streams-queue/
  * TODO: check the license.
  */
object Futures {
  //T is the source type, here String
  //M is the materialization type, here a SourceQueue[String]
  def peekMatValue[T, M](src: Source[T, M]): (Source[T, M], Future[M]) = {
    val p = Promise[M]
    val s = src.mapMaterializedValue { m =>
      p.trySuccess(m)
      m
    }
    (s, p.future)
  }
}
