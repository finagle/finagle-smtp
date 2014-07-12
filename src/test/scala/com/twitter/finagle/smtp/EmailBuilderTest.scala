package com.twitter.finagle.smtp

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import com.twitter.util.Try
import com.twitter.io.TempFile
import java.io.{File, FileOutputStream}

@RunWith(classOf[JUnitRunner])
class EmailBuilderTest extends FunSuite {
  val defaultBuilder = EmailBuilder()

  val testFrom = MailingAddress("from@from")
  val testTo = MailingAddress("to@to")
  val testCc = MailingAddress("cc@cc")
  val testBcc = MailingAddress("bcc@bcc")
  val testReplyTo = MailingAddress("reply@to")

  def mailseq(list: String*): Seq[MailingAddress] = list.seq.map(MailingAddress(_))

  test("build") {
    val testBuilder = new EmailBuilder(Payload(
    from = Seq(testFrom),
    sender = MailingAddress.empty,
    to = Seq(testTo),
    cc = Seq(testCc),
    bcc = Seq(testBcc),
    reply_to = Seq(testReplyTo),
    date = null,
    subject = "subject",
    body = Mime.plainText("body")
    ))

    val built = testBuilder.build

    assert(built.getBcc === Seq(testBcc))
    assert(built.getBody.isInstanceOf[MimePart])
    val mimePartBody = built.getBody.asInstanceOf[MimePart]
    assert(String.copyValueOf(mimePartBody.content.map(_.toChar)) === "body")
    assert(mimePartBody.headers === Map("Content-Type" -> "text/plain"))
    assert(built.getCc === Seq(testCc))
    assert(built.getDate != null, "should default to now")
    assert(built.getFrom === Seq(testFrom))
    assert(built.getReplyTo === Seq(testReplyTo))
    assert(built.getSender.isEmpty, "should not fill Sender field in case of single From")
    assert(built.getSubject === "subject")
    assert(built.getTo === Seq(testTo))
  }

  test("from") {

    val addfrom = defaultBuilder.from("from@from.com").from("from2@from.com")
    assert(addfrom.payload.from.map(_.mailbox) === Seq("from@from.com", "from2@from.com"), "add from")
    val setfrom = addfrom.setFrom(mailseq("from3@from.com"))
    assert(setfrom.payload.from.map(_.mailbox) === Seq("from3@from.com"), "set from")
  }

  test("to") {
    val addto = defaultBuilder.to("to@to.com").to("to2@to.com")
    assert(addto.payload.to.map(_.mailbox) === Seq("to@to.com", "to2@to.com"), "add to")
    val setto = addto.setTo(mailseq("to3@to.com"))
    assert(setto.payload.to.map(_.mailbox) === Seq("to3@to.com"), "set to")
  }

  test("cc") {
    val addcc = defaultBuilder.cc("cc@cc.com").cc("cc2@cc.com")
    assert(addcc.payload.cc.map(_.mailbox) === Seq("cc@cc.com", "cc2@cc.com"), "add cc")
    val setcc = addcc.setCc(mailseq("cc3@cc.com"))
    assert(setcc.payload.cc.map(_.mailbox) === Seq("cc3@cc.com"), "set cc")
  }

  test("bcc") {
    val addbcc = defaultBuilder.bcc("bcc@bcc.com").bcc("bcc2@bcc.com")
    assert(addbcc.payload.bcc.map(_.mailbox) === Seq("bcc@bcc.com", "bcc2@bcc.com"), "add bcc")
    val setbcc = addbcc.setBcc(mailseq("bcc3@bcc.com"))
    assert(setbcc.payload.bcc.map(_.mailbox) === Seq("bcc3@bcc.com"), "set bcc")
  }

  test("reply-to") {
    val addreplyto = defaultBuilder.reply_to("reply@to.com").reply_to("reply2@to.com")
    assert(addreplyto.payload.reply_to.map(_.mailbox) === Seq("reply@to.com", "reply2@to.com"), "add reply-to")
    val setreplyto = addreplyto.setReplyTo(mailseq("reply3@to.com"))
    assert(setreplyto.payload.reply_to.map(_.mailbox) === Seq("reply3@to.com"), "set reply-to")
  }

  test("add single body part") {
    val singlepart = defaultBuilder.addBodyPart(Mime.plainText("part 1"))
    assert(singlepart.payload.body.isInstanceOf[MimeMultipart])
    val mimepart = singlepart.payload.body.asInstanceOf[MimeMultipart]
    assert(new String(mimepart.parts.head.content) === "part 1")
  }

  test("add multiple body parts") {
    val multipart = defaultBuilder.addBodyPart(Mime.plainText("part 1")).addBodyPart(Mime.plainText("part 2"))
    assert(multipart.payload.body.isInstanceOf[MimeMultipart])
    val multimime = multipart.payload.body.asInstanceOf[MimeMultipart]
    assert(multimime.parts.length === 2)
    assert(new String(multimime.parts(0).content) === "part 1")
    assert(new String(multimime.parts(1).content) === "part 2")
  }

  test("set text body") {
    val textmsg = defaultBuilder.text("text")
    val body = textmsg.payload.body.asInstanceOf[MimePart]
    assert(body.contentType === "text/plain")
    assert(new String(body.content) === "text")
  }

  test("set text body with encoding") {
    val textmsg = defaultBuilder.text("text", "UTF-8")
    val body = textmsg.payload.body.asInstanceOf[MimePart]
    assert(body.contentType === "text/plain; charset=\"UTF-8\"")
    assert(new String(body.content) === "text")
  }

  test("attach") {
    val file = File.createTempFile("file", ".txt")
    val os = new FileOutputStream(file)
    os.write("test attachment".getBytes("UTF-8"))
    os.close()

    val msg = defaultBuilder.text("text").attach(file.getAbsolutePath)
    assert(msg.payload.body.isInstanceOf[MimeMultipart])
    val body = msg.payload.body.asInstanceOf[MimeMultipart]
    assert(body.parts(1).contentDisposition === "attachment; filename=\"%s\"".format(file.getName))
    assert(body.parts(1).contentType === "text/plain")
    assert(new String(body.parts(1).content, "UTF-8") === "test attachment")
    file.delete()
  }

  test("sender") {
    val setsender = defaultBuilder.sender("sender@sender.com")
    assert(setsender.payload.sender.mailbox === "sender@sender.com", "add sender")
    assert(setsender.payload.from.map(_.mailbox) === Seq("sender@sender.com"), "add sender to from in case it is the first")
  }
}
