/**
 * Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
 */
package actorbintree

import akka.actor._
import akka.event.LoggingReceive
import akka.pattern.pipe

import scala.collection.immutable.Queue
import scala.util.Random

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

  def createRoot: ActorRef = context.actorOf(BinaryTreeNode.props(0, initiallyRemoved = true), "root-" +  new Random().nextInt(999999999))

  var root = createRoot

  // optional
  var pendingQueue = Queue.empty[Operation]

  // optional
  def receive = normal

  // optional
  /** Accepts `Operation` and `GC` messages. */
  val normal: Receive = LoggingReceive {
    case operation: Operation                      => root ! operation
    case GC                                        => log.info("Starting garbage collection")
                                                      val newRoot = createRoot
                                                      root ! CopyTo(newRoot)
                                                      context.become(garbageCollecting(newRoot))
    case _                                         => println("Error: unknown message received")
  }

  // optional
  /** Handles messages while garbage collection is performed.
    * `newRoot` is the root of the new binary tree where we want to copy
    * all non-removed elements into.
    */
  def garbageCollecting(newRoot: ActorRef): Receive = LoggingReceive {
    case operation: Operation                      => log.info("Garbage collection in process. Enqueuing operation: " + operation)
                                                      pendingQueue = pendingQueue.enqueue(operation)
    case CopyFinished                              => log.debug("Garbage collection finished.")
                                                      root ! PoisonPill
                                                      root = newRoot
                                                      context.become(normal)
                                                      pendingQueue foreach (root ! _)
                                                      pendingQueue = Queue.empty[Operation]
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
  val normal: Receive = LoggingReceive {
    case Insert(requester, operationId, element)   => log.info("Inserting: " + elem)
                                                      insert(requester, operationId, element)
    case Remove(requester, operationId, element)   => log.info("Deleting: " + elem)
                                                      remove(requester, operationId, element)
    case Contains(requester, operationId, element) => log.info("Checking if {} is contained", elem)
                                                      contains(requester, operationId, element)
    case CopyTo(treeNode)                          => log.info("Copying to new tree: ", treeNode)
                                                      copyTo(treeNode)
    case _ => println("Error: unknown message received")
  }

  def insert(requester: ActorRef, operationId: Int, element: Int): Unit = {
    if      (element < this.elem) insertOrCreate(Left, requester, operationId, element)
    else if (element > this.elem) insertOrCreate(Right, requester, operationId, element)
    else {
      this.removed = false
      requester ! OperationFinished(operationId)
    }
  }

  private def insertOrCreate(position: Position, requester: ActorRef, operationId: Int, element: Int): Unit = {
    if (this.subtrees.contains(position)) {
      this.subtrees(position) ! Insert(requester, operationId, element)
    } else {
      this.subtrees += position -> context.actorOf(BinaryTreeNode.props(element, initiallyRemoved = false), "node-" + elem + "-" + new Random().nextInt(999999999))
      requester ! OperationFinished(operationId)
    }
  }

  def remove(requester: ActorRef, operationId: Int, element: Int): Unit = {
    if      (element < this.elem) removeOrIgnore(Left, requester, operationId, element)
    else if (element > this.elem) removeOrIgnore(Right, requester, operationId, element)
    else {
      this.removed = true
      requester ! OperationFinished(operationId)
    }
  }

  private def removeOrIgnore(position: Position, requester: ActorRef, operationId: Int, element: Int): Unit = {
    if (this.subtrees.contains(position)) {
      this.subtrees(position) ! Remove(requester, operationId, element)
    } else {
      requester ! OperationFinished(operationId)
    }
  }

  def contains(requester: ActorRef, operationId: Int, element: Int): Unit = {
    if      (element < this.elem) checkSubtreeIfExists(Left, requester, operationId, element)
    else if (element > this.elem) checkSubtreeIfExists(Right, requester, operationId, element)
    else {
      if (this.removed) requester ! ContainsResult(operationId, false)
      else requester ! ContainsResult(operationId, true)
    }
  }

  def checkSubtreeIfExists(position: Position, requester: ActorRef, operationId: Int, element: Int): Unit = {
    if (this.subtrees.contains(position)) {
      this.subtrees(position) ! Contains(requester, operationId, element)
    } else {
      requester ! ContainsResult(operationId, false)
    }
  }

  def copyTo(treeNode: ActorRef): Unit = {
    if (!this.removed) treeNode ! Insert(self, -elem, elem)
    subtrees.values foreach (_ ! CopyTo(treeNode))
    if (this.removed && subtrees.isEmpty) {
      sender ! CopyFinished
    } else {
      log.debug("Switching node: " + self + " to copying mode")
      context.become(copying(subtrees.values.toSet, false))
    }
  }

  // optional
  /** `expected` is the set of ActorRefs whose replies we are waiting for,
    * `insertConfirmed` tracks whether the copy of this node to the new tree has been confirmed.
    */
  def copying(expected: Set[ActorRef], insertConfirmed: Boolean): Receive = LoggingReceive {
    case OperationFinished(operationId) => context.become(copying(expected, true))
                                           checkIfCopyFinished(expected, true)
    case CopyFinished =>                   context.become(copying(expected - sender, insertConfirmed))
                                           checkIfCopyFinished(expected - sender, insertConfirmed)
  }

  def checkIfCopyFinished(expected: Set[ActorRef], insertConfirmed: Boolean) {
    if (expected.isEmpty && insertConfirmed || expected.isEmpty && removed) {
      context.parent ! CopyFinished
      log.debug("Switching node: " + self + " to normal mode")
      context.become(normal)
    }
  }


}
