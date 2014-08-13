package com.twitter.finagle.smtp

import java.io.{File, FileOutputStream}

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class DefaultEmailTest extends FunSuite {
  val defaultBuilder = DefaultEmail()

  val testFrom = "from@from"
  val testTo = "to@to"
  val testCc = "cc@cc"
  val testBcc = "bcc@bcc"
  val testReplyTo = "reply@to"

  def mailseq(list: String*): Seq[MailingAddress] = list.seq.map(MailingAddress(_))

  test("from") {

    val addfrom = defaultBuilder.from_("from@from.com").from_("from2@from.com")
    assert(addfrom.from.map(_.mailbox) === Seq("from@from.com", "from2@from.com"), "add from")
    val setfrom = addfrom.setFrom(Seq(MailingAddress("from3@from.com")))
    assert(setfrom.from.map(_.mailbox) === Seq("from3@from.com"), "set from")
  }

  test("to") {
    val addto = defaultBuilder.to_("to@to.com").to_("to2@to.com")
    assert(addto.to.map(_.mailbox) === Seq("to@to.com", "to2@to.com"), "add to")
    val setto = addto.setTo(Seq(MailingAddress("to3@to.com")))
    assert(setto.to.map(_.mailbox) === Seq("to3@to.com"), "set to")
  }

  test("cc") {
    val addcc = defaultBuilder.cc_("cc@cc.com").cc_("cc2@cc.com")
    assert(addcc.cc.map(_.mailbox) === Seq("cc@cc.com", "cc2@cc.com"), "add cc")
    val setcc = addcc.setCc(Seq(MailingAddress("cc3@cc.com")))
    assert(setcc.cc.map(_.mailbox) === Seq("cc3@cc.com"), "set cc")
  }

  test("bcc") {
    val addbcc = defaultBuilder.bcc_("bcc@bcc.com").bcc_("bcc2@bcc.com")
    assert(addbcc.bcc.map(_.mailbox) === Seq("bcc@bcc.com", "bcc2@bcc.com"), "add bcc")
    val setbcc = addbcc.setBcc(Seq(MailingAddress("bcc3@bcc.com")))
    assert(setbcc.bcc.map(_.mailbox) === Seq("bcc3@bcc.com"), "set bcc")
  }

  test("reply-to") {
    val addreplyto = defaultBuilder.replyTo_("reply@to.com").replyTo_("reply2@to.com")
    assert(addreplyto.replyTo.map(_.mailbox) === Seq("reply@to.com", "reply2@to.com"), "add reply-to")
    val setreplyto = addreplyto.setReplyTo(Seq(MailingAddress("reply3@to.com")))
    assert(setreplyto.replyTo.map(_.mailbox) === Seq("reply3@to.com"), "set reply-to")
  }


  test("add single body part") {
    val singlepart = defaultBuilder.addBodyPart(Mime.plainText("part 1"))
    assert(singlepart.body.isInstanceOf[MimeMultipart])
    val mimepart = singlepart.body.asInstanceOf[MimeMultipart]
    assert(new String(mimepart.parts.head.content) === "part 1")
  }

  test("add multiple body parts") {
    val multipart = defaultBuilder.addBodyPart(Mime.plainText("part 1")).addBodyPart(Mime.plainText("part 2"))
    assert(multipart.body.isInstanceOf[MimeMultipart])
    val multimime = multipart.body.asInstanceOf[MimeMultipart]
    assert(multimime.parts.length === 2)
    assert(new String(multimime.parts(0).content) === "part 1")
    assert(new String(multimime.parts(1).content) === "part 2")
  }

  test("set text body") {
    val textmsg = defaultBuilder.text("text")
    val body = textmsg.body.asInstanceOf[MimePart]
    assert(body.contentType === "text/plain")
    assert(new String(body.content) === "text")
  }

  test("set text body with encoding") {
    val textmsg = defaultBuilder.text("text", "UTF-8")
    val body = textmsg.body.asInstanceOf[MimePart]
    assert(body.contentType === "text/plain; charset=\"UTF-8\"")
    assert(new String(body.content) === "text")
  }

  test("attach") {
    val file = File.createTempFile("file", ".txt")
    val os = new FileOutputStream(file)
    os.write("test attachment".getBytes("UTF-8"))
    os.close()

    val msg = defaultBuilder.text("text").attach(file.getAbsolutePath)
    assert(msg.body.isInstanceOf[MimeMultipart])
    val body = msg.body.asInstanceOf[MimeMultipart]
    assert(body.parts(1).contentDisposition === "attachment; filename=\"%s\"".format(file.getName))
    assert(body.parts(1).contentType === "text/plain")
    assert(new String(body.parts(1).content, "UTF-8") === "test attachment")
    file.delete()
  }

  test("sender") {
    val setsender = defaultBuilder.sender_("sender@sender.com")
    assert(setsender.sender.mailbox === "sender@sender.com", "add sender")
    assert(setsender.from.map(_.mailbox) === Seq("sender@sender.com"), "add sender to from in case it is the first")
  }
}
