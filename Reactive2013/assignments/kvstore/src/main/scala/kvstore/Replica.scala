package kvstore

import akka.actor._
import kvstore.Arbiter._
import scala.collection.immutable.Queue
import akka.actor.SupervisorStrategy.Restart
import scala.annotation.tailrec
import akka.pattern.{ ask, pipe }
import scala.concurrent.duration._
import akka.util.Timeout
import scala.language.postfixOps
import scala.Some
import kvstore.Arbiter.Replicas
import akka.event.LoggingReceive

object Replica {
  sealed trait Operation {
    def key: String
    def id: Long
  }
  case class Insert(key: String, value: String, id: Long) extends Operation
  case class Remove(key: String, id: Long) extends Operation
  case class Get(key: String, id: Long) extends Operation

  sealed trait OperationReply
  case class OperationAck(id: Long) extends OperationReply
  case class OperationFailed(id: Long) extends OperationReply
  case class GetResult(key: String, valueOption: Option[String], id: Long) extends OperationReply

  def props(arbiter: ActorRef, persistenceProps: Props): Props = Props(new Replica(arbiter, persistenceProps))
}

class Replica(val arbiter: ActorRef, persistenceProps: Props) extends Actor {
  import Replica._
  import Replicator._
  import Persistence._
  import context.dispatcher

  /*
   * The contents of this actor is just a suggestion, you can implement it in any way you like.
   */
  
  var kv = Map.empty[String, String]
  // a map from secondary replicas to replicators
  var secondaries = Map.empty[ActorRef, ActorRef]
  // the current set of replicators
  var replicators = Set.empty[ActorRef]

  var snapshotSeq = 0
  var persistAcks = Map.empty[Long, ActorRef]
  var replicationAcks = Map.empty[Long, (ActorRef, Set[ActorRef])]
  var persistRepeaters = Map.empty[Long, Cancellable]
  var failureGenerators = Map.empty[Long, Cancellable]

  arbiter ! Join

  val persistence = context.actorOf(persistenceProps)

  def receive = LoggingReceive {
    case JoinedPrimary   => context.become(leader)
    case JoinedSecondary => context.become(replica)
  }

  case class GenerateFailure(id: Long)

  /* TODO Behavior for  the leader role. */
  val leader: Receive = LoggingReceive {
    case op:Get => handle(op)
    case op:Insert => handle(op)
    case op:Remove => handle(op)
    case op:Persisted => handle(op)
    case op:Replicated => handle(op)
    case op:GenerateFailure => handle(op)
    case op:Replicas => handle(op)
  }

  /* TODO Behavior for the replica role. */
  val replica: Receive = LoggingReceive {
    case op:Get => handle(op)
    case op:Snapshot => handle(op)
    case op:Persisted => replicaHandle(op)
  }

  def handle(op:Get) = {
    val key = op.key; val id = op.id
    sender ! GetResult(key, kv.get(key), id)
  }

  def handle(op:Insert) = {
    val key = op.key; val value = op.value; val id = op.id
    kv += (key -> value); persistAcks += id -> sender
    if(replicators.nonEmpty) {
      replicationAcks += id -> (sender, replicators)
      replicators.foreach { 
        replicator => replicator ! Replicate(key, Some(value), id)
      }
    }
    persistRepeaters += id -> context.system.scheduler.schedule(
      0 millis, 100 millis, persistence, Persist(key, Some(value), id)
    )
    failureGenerators += id -> context.system.scheduler.scheduleOnce(1 second) {
      self ! GenerateFailure(id)
    }
  }

  def handle(op:Remove) = {
    val key = op.key; val id = op.id
    kv -= key; persistAcks += id -> sender
    if(replicators.nonEmpty) {
      replicationAcks += id -> (sender, replicators)
      replicators.foreach { 
        replicator => replicator ! Replicate(key, None, id)
      }
    }
    persistRepeaters += id -> context.system.scheduler.schedule(
      0 millis, 100 millis, persistence, Persist(key, None, id)
    )
    failureGenerators += id -> context.system.scheduler.scheduleOnce(1 second) {
      self ! GenerateFailure(id)
    }
  }

  def handle(op:Persisted) = { 
    val key = op.key; val id = op.id
    persistRepeaters(id).cancel()
    persistRepeaters -= id
    val origSender = persistAcks(id)
    persistAcks -= id
    if(!replicationAcks.contains(id)) {
      failureGenerators(id).cancel()
      failureGenerators -= id
      origSender ! OperationAck(id)
    }
  }

  def replicaHandle(op:Persisted) = {
    val responder = persistAcks(op.id)
    persistAcks -= op.id
    persistRepeaters(op.id).cancel()
    persistRepeaters -= op.id
    responder ! SnapshotAck(op.key, op.id)
  }

  def handle(op:Replicated) = {
    val key = op.key; val id = op.id
    if(replicationAcks.contains(id)) {
      val (origSender, currAckSet) = replicationAcks(id)
      val newAckSet = currAckSet - sender       
      if (newAckSet.isEmpty) replicationAcks -= id
      else replicationAcks = replicationAcks.updated(id, (origSender, newAckSet))       
      if(!replicationAcks.contains(id) && !persistAcks.contains(id)) {
        failureGenerators(id).cancel()
        failureGenerators -= id
        origSender ! OperationAck(id)
      }
    }
  }

  def handle(op:GenerateFailure) = {
    val id = op.id
    if(failureGenerators.contains(id)) {
      if(persistRepeaters.contains(id)) {
        persistRepeaters(id).cancel()
        persistRepeaters -= id
      }
      failureGenerators -= id      
      val origSender =
        if(persistAcks.contains(id)) persistAcks(id)
        else replicationAcks(id)._1
      persistAcks -= id
      replicationAcks -= id
      origSender ! OperationFailed(id)
    }
  }

  def handle(op:Replicas) = {
    val replicas = op.replicas; 
    val secondaryReplicas = replicas.filterNot(_ == self)
    val removed = secondaries.keySet -- secondaryReplicas
    val added = secondaryReplicas -- secondaries.keySet
    var addedSecondaries = Map.empty[ActorRef, ActorRef]
    val addedReplicators = added.map { replica =>
      val replicator = context.actorOf(Replicator.props(replica))
      addedSecondaries += replica -> replicator
      replicator
    }
    removed.foreach {
      replica => secondaries(replica) ! PoisonPill
    }
    removed.foreach { replica =>
      replicationAcks.foreach { case (id, (origSender, rs)) =>
        if (rs.contains(secondaries(replica))) {
          self.tell(Replicated("", id), secondaries(replica))
        }
      }
    }

    replicators = replicators -- removed.map(secondaries) ++ addedReplicators
    secondaries = secondaries -- removed ++ addedSecondaries

    addedReplicators.foreach { 
      replicator => kv.zipWithIndex.foreach { 
        case ((k,v), idx) => replicator ! Replicate(k, Some(v), idx)
      }
    }
  }

  def handle(op:Snapshot) = {
    if(op.seq < snapshotSeq) {
      sender ! SnapshotAck(op.key, op.seq)
    }
    if (op.seq == snapshotSeq) {
      op.valueOption match {
        case None => kv -= op.key
        case Some(value) => kv += op.key -> value
      }
      snapshotSeq += 1; persistAcks += op.seq -> sender
      persistRepeaters += op.seq -> context.system.scheduler.schedule(
        0 millis, 100 millis, persistence, Persist(op.key, op.valueOption, op.seq)
      )
    }
  }
}