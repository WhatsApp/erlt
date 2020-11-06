package com.whatsapp.akka

import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import akka.actor.typed.scaladsl.Behaviors

import scala.concurrent.{Await, Future}

object GenServerMatchTypes {
  object Gen {
    case class Reply[Reply, State](reply: Reply, state: State)
    case class NoReply[State](state: State)
    sealed trait Req[Call, Cast]
    case class CastReq[Call, Cast](cast: Cast) extends Req[Call, Cast]
    case class CallReq[Call, Cast, Answer](call: Call, from: ActorRef[Answer]) extends Req[Call, Cast]

    def start[Cast, Call, State](gs: GenServer[Cast, Call, State], state: State): GenServerApi[Cast, Call, State] =
      GenServerApi(gs)(ActorSystem(gs.serve(state), gs.name))
    def stop[Cast, Call, State](gsApi: GenServerApi[Cast, Call, State]) =
      gsApi.gRef.terminate()
  }

  trait GenServer[Cast, Call, State] {
    val name: String
    type CallReplyProtocol[C]
    def handleCall[C <: Call](c: C, s: State): CallProtocol[C]
    def handleCast[C <: Cast](c: C, s: State): CastProtocol[C]

    type CallProtocol[C] = Gen.Reply[CallReplyProtocol[C], State]
    type CastProtocol[C] = Gen.NoReply[State]

    def serve(s: State): Behavior[Gen.Req[Call, Cast]] = Behaviors.receive { (context, msg) =>
      msg match
        case req: Gen.CastReq[_, Cast] =>
          serve(handleCast(req.cast, s).state)
        case req: Gen.CallReq[Call, _, CallReplyProtocol[Call]] =>
          val reply = handleCall(req.call, s)
          req.from ! reply.reply
          serve(reply.state)
    }
  }

  case class GenServerApi[Cast, Call, State](gs: GenServer[Cast, Call, State])
                                            (implicit val gRef: ActorSystem[Gen.Req[Call, Cast]])
  {
    import scala.concurrent.duration.DurationInt
    import akka.actor.typed.scaladsl.AskPattern._
    import akka.util.Timeout
    val timeout = 3.seconds

    def cast(c: Cast) =
      gRef ! Gen.CastReq(c)

    def call(c: Call): gs.CallReplyProtocol[Call] =
      implicit val askTimeout: Timeout = timeout
      Await.result(gRef ? (ref => Gen.CallReq(c, ref)), timeout)
  }

  object Counter {
    type State = Int

    enum Cast {
      case Inc()
      case Dec()
    }

    enum Call {
      case Equal(n: Int)
      case Closer(n1: Int, n2: Int)
    }

    sealed trait Reply
    case class EqualReply(equal: Boolean)  extends Reply
    case class CloserReply(n: Int) extends Reply

    type CallReplyProtocol[C] = C match
      case Call.Equal  => EqualReply
      case Call.Closer => CloserReply
  }

  object CounterServer extends GenServer[Counter.Cast, Counter.Call, Counter.State] {
    val name = "CounterServer"
    type CallReplyProtocol[C] = Counter.CallReplyProtocol[C]

    def handleCall[C <: Counter.Call](c: C, s: Counter.State): CallProtocol[C] =
      Gen.Reply(reply(c, s), s)

    def reply[C <: Counter.Call](c: C, s: Counter.State): CallReplyProtocol[C] = c match
      case x: Counter.Call.Equal  =>
        Counter.EqualReply(x.n == s)
      case x: Counter.Call.Closer =>
        val closer = if (Math.abs(x.n1 - s) < Math.abs(x.n2 - s)) then x.n1 else x.n2
        Counter.CloserReply(closer)

    def handleCast[C <: Counter.Cast](c: C, s: Counter.State): CastProtocol[C] = c match
      case x: Counter.Cast.Inc => Gen.NoReply(s + 1)
      case x: Counter.Cast.Dec => Gen.NoReply(s - 1)
  }

  def main(args: Array[String]): Unit =
    val counter = Gen.start(CounterServer, 0)

    counter.cast(Counter.Cast.Inc())
    counter.cast(Counter.Cast.Inc())

    val Counter.EqualReply(eq) = counter.call(Counter.Call.Equal(2))
    println(eq)

    val Counter.CloserReply(i) = counter.call(Counter.Call.Closer(0, 3))
    println(i)

    Gen.stop(counter)

}
