package pl.knirski

import akka.actor.{Actor, ActorSystem, Props}
import cats.data.State
import cats.~>

import scala.concurrent.{Future, Promise}


// Original author: https://gist.github.com/Baccata/e4a200443448f6b09c9c7b6c405a3d89
class ActorInterpreter[F[_], StateData](
    initialData: StateData,
    stateInterpreter: F ~> Lambda[A => State[StateData, A]]
)(implicit actorSystem: ActorSystem) extends (F ~> Future) {

  case class Request[T](value: F[T], promise: Promise[T])

  private val actor = actorSystem.actorOf(Props(new Proxy()))

  override def apply[A](fa: F[A]): Future[A] = {
    val promise = Promise[A]
    actor ! Request(fa, promise)
    promise.future
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
