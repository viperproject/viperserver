package viper.server

import viper.silver.ast.Method
import viper.silver.verifier.VerificationError

object ViperCache {

  private val cache = collection.mutable.Map[String, collection.mutable.Map[String, CacheEntry]]()

  def contains(file: String, m: Method): Boolean = {
    assert(m.info.entityHash != null)
    cache.contains(m.info.entityHash)
  }

  def get(file: String, m: Method): Option[CacheEntry] = {
    assert(m.info.entityHash != null)
    cache.get(file) match {
      case Some(fileCache) =>
        fileCache.get(m.info.entityHash)
      case None => None
    }
  }

  def update(file: String, m: Method, errors: List[VerificationError]): Unit = {
    assert(m.info.entityHash != null)
    cache.get(file) match {
      case Some(fileCache) => fileCache += (m.info.entityHash -> new CacheEntry(errors, m.dependencyHash))
      case None =>
        cache += (file -> collection.mutable.Map[String, CacheEntry]())
        update(file, m, errors)
    }
  }

  def forgetFile(file: String): Unit = {
    cache.remove(file)
  }
}

class CacheEntry(val errors: List[VerificationError], val dependencyHash: String) {}