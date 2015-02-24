package io.github.finagle.smtp

import com.twitter.concurrent.AsyncQueue
import com.twitter.finagle.transport.QueueTransport
import com.twitter.util.Await
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ClientDispatcherTest extends FunSuite {
  def newTestSet = {
    val client = new AsyncQueue[Request]
    val server = new AsyncQueue[UnspecifiedReply]
    val transport = new QueueTransport[Request, UnspecifiedReply](client, server)
    (client, server, transport)
  }

  def newTestSetWithGreeting = {
    val (client, server, transport) = newTestSet
    server.offer(ServiceReady("testdomain","testgreet"))
    val dispatcher = new ClientDispatcher(transport)
    (server, dispatcher)
  }

  test("receives correct greeting") {
    val (server, dispatcher) = newTestSetWithGreeting
    assert(dispatcher.isAvailable)
  }

  test("sends QUIT on close") {
    val (client, server, transport) = newTestSet
    server.offer(ServiceReady("testdomain","testgreet"))
    val dispatcher = new ClientDispatcher(transport)
    dispatcher.close()
    assert(Await.result(client.poll()) match {
      case Request.Quit => true
      case _ => false
    })
    assert(dispatcher.isAvailable, "should wait for QUIT response")
    server.offer(ClosingTransmission("QUIT"))
    assert(!dispatcher.isAvailable)
  }

  test("closes on malformed greeting") {
    val (client, server, transport) = newTestSet
    server.offer(InvalidReply("wronggreet"))
    val dispatcher = new ClientDispatcher(transport)
    dispatcher(Request.Hello)
    server.offer(ClosingTransmission("QUIT"))
    assert(!dispatcher.isAvailable)
  }

  test("aggregates multiline replies") {
    val (server, dispatcher) = newTestSetWithGreeting
    val frep = dispatcher(Request.Noop)

    server.offer(NonTerminalLine(250, "nonterminal"))
    assert(!frep.isDefined)

    server.offer(OK("terminal"))
    val rep = Await result frep

    assert(rep.isMultiline)
    assert(rep.code === 250)
    assert(rep.lines === Seq("nonterminal", "terminal"))
    assert(rep.info === "nonterminal\r\nterminal")
  }

  test("returns specified replies") {
    val (server, dispatcher) = newTestSetWithGreeting
    val specifiedOK = OK("specified")
    val rep = dispatcher(Request.Noop)
    server.offer(specifiedOK)
    assert(Await.result(rep) === specifiedOK)
  }

  test("specifies unspecified replies") {
    val (server, dispatcher) = newTestSetWithGreeting
    val unspecifiedOK = new UnspecifiedReply {
      val info: String = "unspecified"
      val code: Int = 250
    }
    val rep = dispatcher(Request.Noop)
    server.offer(unspecifiedOK)
    assert(Await.result(rep).isInstanceOf[OK])
  }

  test("errors are exceptions") {
    val (server, dispatcher) = newTestSetWithGreeting
    val rep = dispatcher(Request.Noop)
    server.offer(SyntaxError("error"))
    assert(Await.result(rep.liftToTry).isThrow)
  }

  test("wraps unknown replies") {
    val (server, dispatcher) = newTestSetWithGreeting
    val unknownRep = new UnspecifiedReply {
      val info: String = "unknown"
      val code: Int = 666
    }
    val rep = dispatcher(Request.Noop)
    server.offer(unknownRep)

    rep onSuccess { _ =>
      fail("should fail")
    } onFailure {
      case _: UnknownReplyCodeError =>
      case _ => fail("should be UnknownReplyCodeError")
    }
  }
}
