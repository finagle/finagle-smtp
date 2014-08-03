package com.twitter.finagle.smtp

import java.nio.charset.Charset

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class MimeTest extends FunSuite{
  def hasOneHeader(msg: Mime, header: String) = msg.getMimeHeaders.count(_ == header) == 1

  test("simple headers") {
    val mime = Mime.plainText("test")
    assert(mime.getMimeHeaders contains "Content-Type: text/plain")
  }

  test("headers with parameters") {
    val mime = Mime.plainText("test", Charset.forName("UTF-8"))
    assert(mime.getMimeHeaders contains "Content-Type: text/plain; charset=\"UTF-8\"")
  }

  test("Content-Type structure") {
    val ct = ContentType("image", "jpg", Map("x-some-param" -> "value"))
    val mime = Mime.empty.setContentType(ct)

    assert(mime.getMimeHeaders contains "Content-Type: image/jpg; x-some-param=\"value\"")
  }

  test("parse Content-Type") {
    val typestring = "type/subtype; normal-par=normal; qsemicolon=\"; \""
    val ct = ContentType.parse(typestring)
    assert(ct === ContentType("type", "subtype", Map("normal-par" -> "normal", "qsemicolon" -> "; ")))
  }

  test("Content-Disposition structure") {
    val cd = ContentDisposition.attachment("file.png")
    val mime = Mime.empty.setContentDisposition(cd)

    assert(mime.getMimeHeaders contains "Content-Disposition: attachment; filename=\"file.png\"")
  }

  test("MIME-Version is the first header") {
    val headers: Seq[MimeHeader] = Seq(TransferEncoding.Base64, ContentType("application", "octet-stream"), ContentDisposition.inline)
    val mime = Mime.empty addHeaders headers

    assert(mime.getMimeHeaders.head === "MIME-Version: 1.0")
  }

  test("Headers are printed in the same order as they were added") {
    val headers: Seq[MimeHeader] = Seq(TransferEncoding.Base64, ContentType("application", "octet-stream"), ContentDisposition.inline)

    val straightOrder = Mime.empty addHeaders headers

    assert(straightOrder.getMimeHeaders === Seq("MIME-Version: 1.0",
                                                "Content-Transfer-Encoding: base64",
                                                "Content-Type: application/octet-stream",
                                                "Content-Disposition: inline"))

    val reverseOrder = Mime.empty addHeaders headers.reverse

    assert(reverseOrder.getMimeHeaders === Seq("MIME-Version: 1.0",
                                               "Content-Disposition: inline",
                                               "Content-Type: application/octet-stream",
                                               "Content-Transfer-Encoding: base64"))
  }

  test("Multipart MIME is rendered correctly") {
    val text = Mime.plainText("This is a test message.")
    val html = MimePart("<html><h1>This is also a test</h1></html>".getBytes("UTF-8"))
               .setContentTransferEncoding(TransferEncoding.EightBit)
               .setContentType(ContentType("text", "html"))

    val attachment = Mime.plainText("This text is attached")
                      .setContentDisposition(ContentDisposition.attachment("file.txt"))

    val multipart = MimeMultipart(Seq(text, html, attachment))("bnd")

    val expectedMessage = Seq(
    "--bnd",
    "Content-Type: text/plain",
    "",
    "This is a test message.",
    "--bnd",
    "Content-Transfer-Encoding: 8bit",
    "Content-Type: text/html",
    "",
    "<html><h1>This is also a test</h1></html>",
    "--bnd",
    "Content-Type: text/plain",
    "Content-Disposition: attachment; filename=\"file.txt\"",
    "",
    "This text is attached",
    "--bnd--"
    ) mkString "\r\n"

    assert(multipart.message === expectedMessage)
  }

  test("MimePart from file") {
    val bytes = "<html><h1>Test file</h1></html>".getBytes("US-ASCII")
    val file = File.createTempFile("file", ".html")
    val os = new FileOutputStream(file)
    os.write(bytes)
    os.close()

    val mime = Mime.fromFile(file.getAbsolutePath)

    assert(mime.contentType === "text/html")
    assert(mime.content === bytes)

    file.delete()
  }
}
