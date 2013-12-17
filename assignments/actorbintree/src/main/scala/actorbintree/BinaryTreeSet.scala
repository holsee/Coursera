/**
 * Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
 */
package actorbintree

import akka.actor._
import scala.collection.immutable.Queue

object BinaryTreeSet {

  trait Operation {
    def requester: ActorRef
    def id: Int
    def elem: Int
  }

  trait OperationReply {
    def id: Int
  }

  /** Request with identifier `id` to insert an element `elem` into the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Insert(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to check whether an element `elem` is present
    * in the tree. The actor at reference `requester` should be notified when
    * this operation is completed.
    */
  case class Contains(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to remove the element `elem` from the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Remove(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request to perform garbage collection*/
  case object GC

  /** Holds the answer to the Contains request with identifier `id`.
    * `result` is true if and only if the element is present in the tree.
    */
  case class ContainsResult(id: Int, result: Boolean) extends OperationReply
  
  /** Message to signal successful completion of an insert or remove operation. */
  case class OperationFinished(id: Int) extends OperationReply

}


class BinaryTreeSet extends Actor with ActorLogging {
  import BinaryTreeSet._
  import BinaryTreeNode._

  def createRoot: ActorRef = context.actorOf(BinaryTreeNode.props(0, initiallyRemoved = true))

  var root = createRoot

  // optional
  var pendingQueue = Queue.empty[Operation]

  // optional
  def receive = normal

  // optional
  /** Accepts `Operation` and `GC` messages. */
  val normal: Receive = {
    case GC => {
      val newRoot = createRoot
      root ! CopyTo(newRoot)
      context.become(garbageCollecting(newRoot))
    }
    case op:Operation => root ! op
  }

  // optional
  /** Handles messages while garbage collection is performed.
    * `newRoot` is the root of the new binary tree where we want to copy
    * all non-removed elements into.
    */
  def garbageCollecting(newRoot: ActorRef): Receive = {
    case CopyFinished => {
      root ! PoisonPill
      root = newRoot
      pendingQueue.foreach { newRoot ! _ }
      pendingQueue = Queue.empty[Operation]
      context.unbecome()
    }
    case op:Operation => pendingQueue = pendingQueue :+ op
  }
}

object BinaryTreeNode {
  trait Position

  case object Left extends Position
  case object Right extends Position

  case class CopyTo(treeNode: ActorRef)
  case object CopyFinished

  def props(elem: Int, initiallyRemoved: Boolean) = Props(classOf[BinaryTreeNode],  elem, initiallyRemoved)
}

class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor with ActorLogging {
  import BinaryTreeNode._
  import BinaryTreeSet._

  var subtrees = Map[Position, ActorRef]()
  var removed = initiallyRemoved

  // optional
  def receive = normal

  // optional
  /** Handles `Operation` messages and `CopyTo` requests. */
  val normal: Receive = {
    case op:Contains => contains(op)
    case op:Insert => insert(op)
    case op:Remove => remove(op)
    case CopyTo(node) => copyTo(node)
  }

  // optional
  /** `expected` is the set of ActorRefs whose replies we are waiting for,
    * `insertConfirmed` tracks whether the copy of this node to the new tree has been confirmed.
    */
  def copying(expected: Set[ActorRef], insertConfirmed: Boolean): Receive ={
    case OperationFinished(elem) => {
      context.become(copying(expected, true))
      if(expected.isEmpty)
        self ! CopyFinished
    }
    case CopyFinished => {
      val rest = expected - sender
      if(rest.isEmpty && insertConfirmed) {
        context.parent ! CopyFinished
      } else {
        context.become(copying(rest, insertConfirmed))
      }
    }
  }

  def copyTo(node:ActorRef) = {
    val expected = subtrees.values.toSet
    context become copying(expected, false)
    if(!removed)
      node ! Insert(self, elem, elem)
    else
      self ! OperationFinished(elem)
    expected.foreach(_ ! CopyTo(node))  
  }

  private def path(e:Int) = if (e < elem) Left else Right

  def contains(op:Operation) = op.elem match {
    case e if (e == elem) => op.requester ! ContainsResult(op.id, !removed)
    case e => delegateContains(op, path(e))
  }

  def delegateContains(op:Operation, pos:Position) = {
    if(subtrees contains pos){
      subtrees(pos) ! op
    } else {
      op.requester ! ContainsResult(op.id, false)
    }
  }

  def insert(op:Operation) = op.elem match {
    case e if (e == elem) => { 
      removed = false
      op.requester ! OperationFinished(op.id) 
    }
    case e => delegateInsert(op, path(e)) 
  }

  def delegateInsert(op:Operation, pos:Position) = {
    if(subtrees contains pos){
      subtrees(pos) ! op
    } else {
      subtrees = subtrees + (pos -> context.actorOf(BinaryTreeNode.props(op.elem, initiallyRemoved = false)))
      op.requester ! OperationFinished(op.id)
    }
  }

  def remove(op:Operation) = op.elem match {
    case e if (e == elem) => {
      removed = true
      op.requester ! OperationFinished(op.id)
    }
    case e => delegateRemove(op, path(e)) 
  }

  def delegateRemove(op:Operation, pos:Position) = {
    if(subtrees contains pos){
      subtrees(pos) ! op
    } else {
      op.requester ! OperationFinished(op.id)
    }
  }
}

