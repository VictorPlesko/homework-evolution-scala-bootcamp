package by.plesko.bootcamp.akka

import akka.actor.{Actor, ActorRef, Props}

object BinaryTreeNode {
  private sealed trait Position

  private case object Left extends Position
  private case object Right extends Position

  def props(elem: Int, initiallyRemoved: Boolean): Props = Props(new BinaryTreeNode(elem, initiallyRemoved))
}

final class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor {

  import BinaryTreeNode._
  import BinaryTreeSet.Operation._
  import BinaryTreeSet.OperationReply._

  private var subtrees = Map[Position, ActorRef]()
  private var removed = initiallyRemoved

  override def receive: Receive = {
    case insert: Insert => doInsert(insert)
    case contains: Contains => doContains(contains)
    case remove: Remove => doRemove(remove)
  }

  private def getPosition(mElem: Int): Option[ActorRef] = if (mElem < elem) subtrees.get(Left) else subtrees.get(Right)

  private def doInsert(m: Insert): Unit = {
    if (m.elem == elem) {
      removed = false
      m.requester ! OperationFinished(m.id)
    } else if (m.elem < elem) {
      subtrees.get(Left).fold {
        subtrees = subtrees + (Left -> context.actorOf(props(m.elem, initiallyRemoved = false)))
        m.requester ! OperationFinished(m.id)
      }(_ ! m)
    } else {
      subtrees.get(Right).fold {
        subtrees = subtrees + (Right -> context.actorOf(props(m.elem, initiallyRemoved = false)))
        m.requester ! OperationFinished(m.id)
      }(_ ! m)
    }
  }

  private def doContains(m: Contains): Unit = {
    val position: Option[ActorRef] = getPosition(m.elem)
    if (m.elem == elem && !removed) m.requester ! ContainsResult(m.id, result = true)
    else position.fold(m.requester ! ContainsResult(m.id, result = false))(_ ! m)
  }

  private def doRemove(m: Remove): Unit = {
    val position: Option[ActorRef] = getPosition(m.elem)
    if (m.elem == elem) {
      removed = true
      m.requester ! OperationFinished(m.id)
    } else
      position.fold(m.requester ! OperationFinished(m.id))(_ ! m)
  }
}
