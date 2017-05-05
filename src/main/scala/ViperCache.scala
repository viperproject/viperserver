package viper.server

import viper.carbon.CarbonVerifier
import viper.silver.ast.Method
import viper.silver.verifier.{VerificationError, Verifier}

object ViperCache {
  //TODO: take config.backendSpecificCache() into account

  private val cache = collection.mutable.Map[String, collection.mutable.Map[String, CacheEntry]]()

  private var _backendSpecificCache: Boolean = false

  def initialize(backendSpecificCache: Boolean): Unit = {
    _backendSpecificCache = backendSpecificCache
  }

  def contains(backendName: String, file: String, m: Method): Boolean = {
    get(backendName, file, m).isDefined
  }

  def get(backendName: String, file: String, m: Method): Option[CacheEntry] = {
    assert(m.info.entityHash != null)
    val key = getKey(backendName, file)
    cache.get(key) match {
      case Some(fileCache) =>
        fileCache.get(m.info.entityHash)
      case None => None
    }
  }

  def update(backendName: String, file: String, m: Method, errors: List[VerificationError]): Unit = {
    assert(m.info.entityHash != null)
    val key = getKey(backendName, file)
    cache.get(key) match {
      case Some(fileCache) => fileCache += (m.info.entityHash -> new CacheEntry(errors, m.dependencyHash))
      case None =>
        cache += (key -> collection.mutable.Map[String, CacheEntry]())
        update(backendName, file, m, errors)
    }
  }

  def forgetFile(backendName: String, file: String): Unit = {
    val key = getKey(backendName, file)
    cache.remove(key)
  }

  private def getKey(backendName: String, file: String): String = (if (_backendSpecificCache) backendName else "") + file

  def resetCache(): Unit = {
    cache.clear()
  }
}

class CacheEntry(val errors: List[VerificationError], val dependencyHash: String) {}