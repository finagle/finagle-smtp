package io.github.finagle.smtp

import com.twitter.concurrent.AsyncQueue
import com.twitter.finagle.Service
import com.twitter.finagle.transport.QueueTransport
import com.twitter.util.{Await, Future}
import io.github.finagle.smtp.extension._
import io.github.finagle.smtp.extension.auth._
import io.github.finagle.smtp.extension.binarymime.NoBinaryMimeFilter
import io.github.finagle.smtp.extension.chunking.{ChunkingFilter, ChunkingReq, NoChunkingFilter}
import io.github.finagle.smtp.extension.eightbitmime.{EightBitMimeFilter, NoEightBitMimeFilter}
import io.github.finagle.smtp.extension.pipelining.{NoPipeliningFilter, PipeliningFilter, RequestGroup}
import io.github.finagle.smtp.extension.size.{NoSizeDeclarationFilter, SizeDeclarationFilter}
import io.github.finagle.smtp.util._
import org.jboss.netty.util.CharsetUtil
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class NoEightBitMimeTest extends FunSuite {
  test("removes BODY=8BITMIME extension from MAIL FROM") {
    val sender = MailingAddress("test@t.com")
    val exreq = ExtendedMailingSession(sender).bodyEncoding(BodyEncoding.EightBit)

    val service = NoEightBitMimeFilter andThen SimpleTestService
    val rep = Await.result(service(exreq)).asInstanceOf[TestReply]
    val req = rep.req.asInstanceOf[ExtendedMailingSession]
    assert(!req.ext.contains("BODY"))
  }

  test("does not remove BODY!=8BITMIME extension from MAIL FROM") {
    val sender = MailingAddress("test@t.com")
    val exreq = ExtendedMailingSession(sender).bodyEncoding(BodyEncoding.SevenBit)

    val service = NoEightBitMimeFilter andThen SimpleTestService
    val rep = Await.result(service(exreq)).asInstanceOf[TestReply]
    val req = rep.req.asInstanceOf[ExtendedMailingSession]
    assert(req.ext.contains("BODY"))
  }

  test("rejects requests with 8-bit text data with RequestNotAllowed") {
    val text = Request.TextData(Seq("test"), CharsetUtil.UTF_8)
    val service = NoEightBitMimeFilter andThen SimpleTestService

    val textrep = service(text) onSuccess { rep =>
      fail("should not have accepted 8-bit MIME data")
    } onFailure {
      case _: RequestNotAllowed =>
      case _ => fail("should be RequestNotAllowed")
    }
  }

  test("rejects requests with 8-bit MIME data with RequestNotAllowed") {
    val mime = Request.MimeData(Mime.plainText("test", CharsetUtil.UTF_8))
    val service = NoEightBitMimeFilter andThen SimpleTestService

    val mimerep = service(mime) onSuccess { rep =>
      fail("should not have accepted 8-bit MIME data")
    } onFailure {
      case _: RequestNotAllowed =>
      case _ => fail("should be RequestNotAllowed")
    }
  }
}

class EightBitMimeTest extends FunSuite {
    test("transforms 8-bit text data into MIME") {
      val text = Request.TextData(Seq("test"), CharsetUtil.UTF_8)
      val service = EightBitMimeFilter andThen SimpleTestService

      val rep = Await.result(service(text)).asInstanceOf[TestReply]
      assert(rep.req.isInstanceOf[Request.MimeData])
      val mime = rep.req.asInstanceOf[Request.MimeData].data
      assert(mime.headers.getOrElse("Content-Type", "none") startsWith "text/plain")
      assert(mime.headers.getOrElse("Content-Transfer-Encoding", "none") === "8bit")
    }
}

class NoSizeDeclarationTest extends FunSuite {
  test("removes SIZE extension from MAIL FROM") {
    val sender = MailingAddress("test@t.com")
    val exreq = ExtendedMailingSession(sender).messageSize(4)

    val service = NoSizeDeclarationFilter andThen SimpleTestService
    val rep = Await.result(service(exreq)).asInstanceOf[TestReply]
    assert(rep.req.isInstanceOf[ExtendedMailingSession])
    val req = rep.req.asInstanceOf[ExtendedMailingSession]
    assert(!req.ext.contains("SIZE"))
  }
}

class SizeDeclarationTest extends FunSuite {
  test("rejects requests to transmit oversized messages with InsufficientStorageError") {
    val sender = MailingAddress("test@t.com")
    val req = ExtendedMailingSession(sender).messageSize(10)
    val service = new SizeDeclarationFilter(5) andThen SimpleTestService

    val mimerep = service(req) onSuccess { rep =>
      fail("should not accept oversized message")
    } onFailure {
      case _: InsufficientStorageError =>
      case _ => fail("should be InsufficientStorageError")
    }
  }

  test("ignores size constraints when SIZE=0") {
    val sender = MailingAddress("test@t.com")
    val req = ExtendedMailingSession(sender).messageSize(10)
    val service = new SizeDeclarationFilter(0) andThen SimpleTestService

    val mimerep = service(req) onFailure { _ => fail("should pass the request") }
  }
}

class NoChunkingTest extends FunSuite {
  test("rejects BeginDataChunk with RequestNotAllowed") {
    val service = NoChunkingFilter andThen SimpleTestService
    val chunkrep = service(ChunkingReq.BeginDataChunk(5)) onSuccess { rep =>
      fail("should not accept chunking")
    } onFailure {
      case _: RequestNotAllowed =>
      case _ => fail("should be RequestNotAllowed")
    }
  }

  test("rejects BeginLastDataChunk with RequestNotAllowed") {
    val service = NoChunkingFilter andThen SimpleTestService
    val chunkrep = service(ChunkingReq.BeginLastDataChunk(5)) onSuccess { rep =>
      fail("should not accept chunking")
    } onFailure {
      case _: RequestNotAllowed =>
      case _ => fail("should be RequestNotAllowed")
    }
  }
}

class ChunkingTest extends FunSuite {
  test("resets the session in case of failed BDAT and DATA commands") {
    val resets = new AsyncQueue[Reply]

    val testService = new Service[Request, Reply] {
      def apply(request: Request) = request match {
        case ChunkingReq.BeginDataChunk(_) => Future.exception(ProcessingError("chunk"))
        case ChunkingReq.BeginLastDataChunk(_) => Future.exception(ProcessingError("last"))
        case Request.BeginData => Future.exception(BadCommandSequence("data"))
        case Request.Reset => {
          resets.offer(OK("reset"))
          Future.value(TestReply(request))
        }
        case _ => Future.value(OK("other"))
      }
    }
    val service = ChunkingFilter andThen testService

    service(ChunkingReq.BeginDataChunk(5))
    assert(resets.size === 1)
    service(ChunkingReq.BeginLastDataChunk(5))
    assert(resets.size === 2)
    service(Request.BeginData)
    assert(resets.size === 3)
  }

  test("reply to RSET is read before the next request") {
    val client = new AsyncQueue[Request]
    val server = new AsyncQueue[UnspecifiedReply]
    val transport = new QueueTransport[Request, UnspecifiedReply](client, server)
    server.offer(ServiceReady("testdomain","testgreet"))
    val dispatcher = new ClientDispatcher(transport)
    val service = new Service[Request, Reply] {
      def apply(request: Request) = dispatcher(request)
    }
    val chunkService = ChunkingFilter andThen service

    val rep = chunkService(Request.BeginData)
    server.offer(BadCommandSequence("data"))
    server.offer(OK("rset"))
    assert(Await.result(rep.liftToTry).isThrow)
    val rep1 = chunkService(Request.Noop)
    server.offer(Help("noop"))
    assert(Await.result(rep1).isInstanceOf[Help])
  }
}

class NoBinaryMimeTest extends FunSuite {
  test("removes BODY=BINARYMIME extension from MAIL FROM") {
    val sender = MailingAddress("test@t.com")
    val exreq = ExtendedMailingSession(sender).bodyEncoding(BodyEncoding.Binary)

    val service = NoBinaryMimeFilter andThen SimpleTestService
    val rep = Await.result(service(exreq)).asInstanceOf[TestReply]
    val req = rep.req.asInstanceOf[ExtendedMailingSession]
    assert(!req.ext.contains("BODY"))
  }

  test("does not remove BODY!=BINARYMIME extension from MAIL FROM") {
    val sender = MailingAddress("test@t.com")
    val exreq = ExtendedMailingSession(sender).bodyEncoding(BodyEncoding.EightBit)

    val service = NoBinaryMimeFilter andThen SimpleTestService
    val rep = Await.result(service(exreq)).asInstanceOf[TestReply]
    val req = rep.req.asInstanceOf[ExtendedMailingSession]
    assert(req.ext.contains("BODY"))
  }

  test("rejects binary MIME messages with RequestNotAllowed") {
    val mime = Request.MimeData(Mime.plainText("test", CharsetUtil.UTF_8)
                                .setContentTransferEncoding(TransferEncoding.Binary))
    val service = NoBinaryMimeFilter andThen SimpleTestService

    val mimerep = service(mime) onSuccess { rep =>
      fail("should not have accepted binary MIME data")
    } onFailure {
      case _: RequestNotAllowed =>
      case _ => fail("should be RequestNotAllowed")
    }
  }
}

class NoPipeliningTest extends FunSuite {
  test("rejects grouped requests with RequestNotAllowed") {
    val grouped = RequestGroup(Request.Hello, Request.Quit)
    val service = NoPipeliningFilter andThen SimpleTestService

    val rep = service(grouped) onSuccess { rep =>
      fail("should not have accepted grouped request")
    } onFailure {
      case _: RequestNotAllowed =>
      case _ => fail("should be RequestNotAllowed")
    }
  }
}

class PipeliningTest extends FunSuite {
  test("rejects group requests with the wrong order of commands with BadCommandSequence reply") {
    val grouped = RequestGroup(Request.Hello, ExtendedMailingSession(MailingAddress.empty), Request.Noop)
    val service = PipeliningFilter andThen SimpleTestService

    val rep = service(grouped) onSuccess { rep =>
      fail("should not have accepted wrong-ordered grouped request")
    } onFailure {
      case BadCommandSequence(_) =>
      case _ => fail("should be BadCommandSequence")
    }
  }

  test("allows correct group requests") {
    val grouped = RequestGroup(ExtendedMailingSession(MailingAddress.empty), Request.BeginData)
    val service = PipeliningFilter andThen SimpleTestService

    val rep = service(grouped) onFailure { _ => fail("should allow correct request") }
  }
}

class NoAuthTest extends FunSuite {
  test("removes AUTH extension from MAIL FROM") {
    val sender = MailingAddress("test@t.com")
    val exreq = ExtendedMailingSession(sender).authorize(sender)

    val service = NoAuthFilter andThen SimpleTestService
    val rep = Await.result(service(exreq)).asInstanceOf[TestReply]
    val req = rep.req.asInstanceOf[ExtendedMailingSession]
    assert(!req.ext.contains("AUTH"))
  }

  test("rejects AUTH request") {
    val service = NoAuthFilter andThen SimpleTestService
    service(AuthRequest(AuthMechanism.plain("login", "password"))) onSuccess { _ =>
      fail("should not have accepted auth request")
    }

  }
}

class AuthTest extends FunSuite {
  val improperChallenge = "¥£€$¢₡₢₣₤₥₦₧₨₩₪₫₭₮₯₹"

  // This service sends improperly encoded challenge
  // in all cases except auth cancellation
  val challengeService = new Service[Request, Reply] {
    def apply(req: Request) = req match {
      case ChallengeResponse.cancelAuth => Future.value(OK("cancel"))
      case _ => Future.value(ServerChallenge(improperChallenge))
    }
  }

  val authService = AuthFilter andThen challengeService

  test("rejects improperly encoded challenges") {
    val rep = Await result authService(ChallengeResponse("resp"))
    assert(rep.isInstanceOf[OK])
  }

  test("converts unexpected challenges to InvalidReply") {
    val rep = authService(Request.Hello) onSuccess { _ =>
      fail("should fail")
    } onFailure {
      case InvalidReply(_) =>
      case _ => fail("should be InvalidReply")
    }
  }
}
