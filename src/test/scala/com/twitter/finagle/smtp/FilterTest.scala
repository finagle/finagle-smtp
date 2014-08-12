package com.twitter.finagle.smtp

import java.net.InetAddress
import java.text.SimpleDateFormat
import com.twitter.finagle.Service
import com.twitter.finagle.smtp.extension.ExtendedMailingSession
import com.twitter.finagle.smtp.filter.{SmtpLoggingFilter, DataFilter, HeadersFilter, MailFilter}
import com.twitter.finagle.smtp.util._
import com.twitter.logging.{BareFormatter, StringHandler, Logger}
import com.twitter.util.{Await, Future}
import org.jboss.netty.util.CharsetUtil
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner


@RunWith(classOf[JUnitRunner])
class DataFilterTest extends FunSuite {
  val dataFilterService = DataFilter andThen SimpleTestService

  test("makes text data end with <CRLF>.<CRLF>") {
    val data = Seq("line1", "line2.")
    val request = Request.TextData(data)
    val response = Await.result(dataFilterService(request)).asInstanceOf[TestReply]
    assert(response.req.asInstanceOf[Request.TextData].cmd === "line1\r\nline2.\r\n.") //last /r/n will be added by encoder,
                                                                                       // as after any other text command
  }

  test("makes byte data end with <CRLF>.<CRLF>") {
    val data = "line1\r\nline2.".getBytes("US-ASCII")
    val request = Request.MimeData(MimePart(data))
    val response = Await.result(dataFilterService(request)).asInstanceOf[TestReply]
    val repContent = new String(response.req.asInstanceOf[Request.MimeData].data.content, CharsetUtil.US_ASCII)
    assert(repContent === "line1\r\nline2.\r\n.\r\n") //last /r/n should be present, as it will not be added by encoder
  }

  test("duplicates leading dot in text data") {
    val data = Seq(".", ".line1", "line2.")
    val request = Request.TextData(data)
    val response = Await.result(dataFilterService(request)).asInstanceOf[TestReply]
    assert(response.req.asInstanceOf[Request.TextData].cmd === "..\r\n..line1\r\nline2.\r\n.") //last /r/n will be added by encoder, as after any other command
  }

  test("duplicates leading dot in byte data") {
    val data = ".\r\n.line1\r\nline2.".getBytes("US-ASCII")
    val request = Request.MimeData(MimePart(data))
    val response = Await.result(dataFilterService(request)).asInstanceOf[TestReply]
    val repContent = new String(response.req.asInstanceOf[Request.MimeData].data.content, CharsetUtil.US_ASCII)
    assert(repContent === "..\r\n..line1\r\nline2.\r\n.\r\n") //last /r/n should be present, as it will not be added by encoder
  }

  test("ignores non-Data commands") {
    val req1 = Request.Hello
    val req2 = ExtendedMailingSession(MailingAddress("test@test.test"))
    val rep1 = Await.result(dataFilterService(req1)).asInstanceOf[TestReply]
    assert(rep1.req === req1)
    val rep2 = Await.result(dataFilterService(req2)).asInstanceOf[TestReply]
    assert(rep2.req === req2)
  }
}


@RunWith(classOf[JUnitRunner])
class MailFilterTest extends FunSuite {
  test("transforms email to right command sequence") {
  def MailTestService(msg: EmailMessage) = new Service[Request, Reply] {
    var cmdSeq = Seq(
      Request.Hello,
      ExtendedMailingSession(msg.sender)) ++
      msg.to.map(Request.AddRecipient(_)) ++ Seq(
      Request.BeginData,
      Request.TextData(Seq("MIME-Version: 1.0", "Content-Type: text/plain","body")),
      Request.Quit
    )

    def apply(req: Request): Future[Reply] = {
      assert(req === cmdSeq.head)
      if (req == cmdSeq.head) {
        cmdSeq = cmdSeq.tail
        Future { TestReply(req) }
      }
      else Future.exception(new TestError)
    }
  }

    val msg = DefaultEmail()
              .from_("from@test.com")
              .to_("to@test.com")
              .subject_("test")
              .text("body")

    val mailFilterService = MailFilter andThen MailTestService(msg)
    val test = mailFilterService(msg)
  }
}

@RunWith(classOf[JUnitRunner])
class HeaderFilterTest extends FunSuite {
  def hasOneHeader(lines: Seq[String], header: String): Boolean = lines.count(_.startsWith(header)) == 1
  def checkHeader(body: Seq[String], header: String, expected: String) = {
    val line = body.find(_.startsWith(header))
    line match {
      case Some(hdr) => {
        val value = hdr drop header.length
        assert(value === expected, "%s is incorrect" format header)
      }
      case None => throw new TestError
    }
  }

  val simpleMsg = DefaultEmail()
    .from_("from@test.com")
    .to_("to@test.com")
    .subject_("test")
    .text("body")

  val multipleAddressMsg = DefaultEmail()
    .from_("from1@test.com", "from2@test.com")
    .to_("to1@test.com", "to2@test.com")
    .subject_("test")
    .text("body")

  test("result message has all necessary headers") {
    val headerTestService = new Service[EmailMessage, Unit] {
      def apply(msg: EmailMessage): Future[Unit] = Future {
        val body = msg.body
        val lines = body.getMimeHeaders ++ body.message.split("\r\n")
        val headers = Seq("From: ", "To: ", "Subject: ", "Date: ")
        val hasHeaders = headers.map(hasOneHeader(lines, _)).reduce(_ && _)
        assert(hasHeaders)
      }
    }

    val headerFilterService = HeadersFilter andThen headerTestService
    val test = headerFilterService(simpleMsg)
  }

  test("message has Sender header in case of multiple From") {
    val headerTestService = new Service[EmailMessage, Unit] {
      def apply(msg: EmailMessage): Future[Unit] = {
        val body = msg.body
        val lines = body.getMimeHeaders ++ body.message.split("\r\n")
        if (msg.from.length > 1) Future  {
          assert(hasOneHeader(lines, "Sender: "))
          checkHeader(lines, "Sender: ", msg.sender.mailbox)
        }
        else Future.Done
      }
    }

    val headerFilterService = HeadersFilter andThen headerTestService
    val test = Await result headerFilterService(multipleAddressMsg)
  }

  test("necessary headers correspond to the right values") {
    val headerTestService = new Service[EmailMessage, Unit] {
      def apply(msg: EmailMessage): Future[Unit] = Future {
        val body = msg.body
        val lines = body.getMimeHeaders ++ body.message.split("\r\n")

        checkHeader(lines, "From: ", msg.from.map(_.mailbox).mkString(","))
        checkHeader(lines, "To: ", msg.to.map(_.mailbox).mkString(","))
        checkHeader(lines, "Subject: ", msg.subject)
        checkHeader(lines, "Date: ", EmailMessage.DateFormat.format(msg.date))
      }
    }

    val headerFilterService = HeadersFilter andThen headerTestService
    val test = headerFilterService(multipleAddressMsg)
  }
}

@RunWith(classOf[JUnitRunner])
class SmtpLoggingFilterTest extends FunSuite {
  test("logs successful answers") {
    val successfulService = new Service[Request, Reply] {
      def apply(req: Request): Future[Reply] = Future.value(OK("ok"))
    }

    val log = Logger.get("test")
    log.setLevel(Logger.INFO)
    val stringHandler = new StringHandler(BareFormatter, Some(Logger.INFO))
    log.addHandler(stringHandler)
    log.setUseParentHandlers(false)

    val loggingService = new SmtpLoggingFilter(log) andThen successfulService

    Await result loggingService(Request.Hello)

    assert(stringHandler.get === "Client: EHLO %s\r\nServer: 250 ok\n".format(InetAddress.getLocalHost.getHostName))
  }

  test("logs errors") {
    val unsuccessfulService = new Service[Request, Reply] {
      def apply(req: Request): Future[Reply] = Future.exception(SyntaxError("err"))
    }

    val log = Logger.get("test")
    log.setLevel(Logger.INFO)
    val stringHandler = new StringHandler(BareFormatter, Some(Logger.INFO))
    log.addHandler(stringHandler)
    log.setUseParentHandlers(false)

    val loggingService = new SmtpLoggingFilter(log) andThen unsuccessfulService

    loggingService(Request.Hello) rescue {
      case _ => Future.Done
    }

    assert(stringHandler.get === "Client: EHLO %s\r\nServer: 500 err\n".format(InetAddress.getLocalHost.getHostName))
  }
}