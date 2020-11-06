package com.whatsapp.akka

import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import akka.actor.typed.scaladsl.Behaviors

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.DurationInt

object BasicBlocks {

  sealed trait Request

  sealed trait CastRequest extends Request
  case object Inc extends CastRequest
  case object Dec extends CastRequest

  sealed trait CallRequest extends Request
  case class Equal(i: Int, replyTo: ActorRef[EqualResponse]) extends CallRequest
  case class Closer(i1: Int, i2: Int, replyTo: ActorRef[CloserResponse])
      extends CallRequest

  case class EqualResponse(response: Boolean)
  case class CloserResponse(response: Int)

  // There is no separation of handle_cast/handle_call here
  def countLoopFunStyle(count: Int): Behavior[Request] =
    Behaviors.receive { (context, msg) =>
      msg match {
        case Inc =>
          context.log.info("Got Inc")
          countLoopFunStyle(count + 1)
        case Dec =>
          context.log.info("Got Dec")
          countLoopFunStyle(count - 1)
        case Equal(i, replyTo) =>
          context.log.info(s"Got Equal($i)")
          replyTo ! EqualResponse(i == count)
          countLoopFunStyle(count)
        case Closer(i1, i2, replyTo) =>
          context.log.info(s"Got Closer($i1, $i2)")
          val resp = if (Math.abs(i1 - count) < Math.abs(i2 - count)) i1 else i2
          replyTo ! CloserResponse(resp)
          countLoopFunStyle(count)
      }
    }

  def main(args: Array[String]): Unit = {
    implicit val actorSystem: ActorSystem[Request] =
      ActorSystem(BasicBlocks.countLoopFunStyle(0), "BasicCounter")

    {
      // Simple fire and forget ~~~ gen_server:cast
      actorSystem ! Inc
      actorSystem ! Inc
    }

    {
      // Ask Pattern ~~~ gen_server:call
      // Ask and await synchronously
      import akka.actor.typed.scaladsl.AskPattern._
      import akka.util.Timeout

      // asking someone requires a timeout if the timeout hits without response
      // the ask is failed with a TimeoutException
      implicit val timeout: Timeout = 3.seconds

      val fResult: Future[CloserResponse] =
        actorSystem ? (ref => Closer(1, 2, ref))

      // Trying to mimic the "Erlangism" of synchronous calls here.
      // Await.result IS NOT idiomatic Akka.
      // In idiomatic Akka, it is more encouraged to combine the things, - like futures,
      // or to follow the RX path.
      val result =
        Await.result(fResult, 3.seconds)

      actorSystem.log.info(s"The result: $result")
    }

    actorSystem.terminate()
  }
}
