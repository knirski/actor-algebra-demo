package pl.knirski

import akka.actor.{Actor, ActorSystem, Props}
import cats.data.State
import cats.~>

import scala.concurrent.{Future, Promise}


// Original author: https://gist.github.com/Baccata/e4a200443448f6b09c9c7b6c405a3d89
class ActorInterpreter[G[_], StateData](
    initialData: StateData,
    stateInterpreter: G ~> Lambda[A => State[StateData, A]]
)(implicit actorSystem: ActorSystem) extends (G ~> Future) {

  case class Request[T](value: G[T], promise: Promise[T])

  private val actor = actorSystem.actorOf(Proxy.props())

  override def apply[A](fa: G[A]): Future[A] = {
    val promise = Promise[A]
    actor ! Request(fa, promise)
    promise.future
  }

  object Proxy {
    def props(): Props = Props(new Proxy())
  }

  private class Proxy extends Actor {

    var data: StateData = initialData

    override def receive: Receive = {
      case Request(instruction, promise) =>
        val res = stateInterpreter.apply(instruction).run(data)
        data = res.value._1
        promise.success(res.value._2)
    }
  }

}
