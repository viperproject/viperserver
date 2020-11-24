package viper.server.vsi

import akka.actor.ActorRef
import akka.stream.scaladsl.SourceQueueWithComplete
import org.reactivestreams.Publisher

import scala.collection.mutable
import scala.concurrent.Future

case class JobID(id: Int)
case class JobHandle(job_actor: ActorRef,
                     queue: SourceQueueWithComplete[Envelope],
                     publisher: Publisher[Envelope])

/** This class manages the verification jobs the server receives.
  */
class JobPool(val MAX_ACTIVE_JOBS: Int = 3) {
  var jobHandles: mutable.Map[Int, Future[JobHandle]] = mutable.Map[Int, Future[JobHandle]]()
  private var _nextJobId: Int = 0

  def newJobsAllowed = jobHandles.size < MAX_ACTIVE_JOBS

  /** Creates a Future of a JobHandle.
    *
    * For the next available job ID the function job_executor will set up a JobActor. That actor
    * will start a verification process and produce a Future JobHandle. The Future will
    * successfully complete as soon as the verification process was started successfully.
    * */
  def bookNewJob(job_executor: Int => Future[JobHandle]): (Int, Future[JobHandle]) = {
    val new_jid = _nextJobId
    jobHandles(new_jid) = job_executor(new_jid)
    _nextJobId = _nextJobId + 1
    (new_jid, jobHandles(new_jid))
  }

  /** Discards the JobHandle for the given JobID
    * */
  def discardJob(jid: JobID): mutable.Map[Int, Future[JobHandle]] = {
    jobHandles -= jid.id
  }

  def lookupJob(jid: JobID): Option[ Future[JobHandle] ] = {
    jobHandles.get(jid.id)
  }
}