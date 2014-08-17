package com.twitter.finagle.smtp

import java.io.File
import java.nio.charset.Charset

import com.twitter.io.Files
import org.jboss.netty.util.CharsetUtil

/*A trait for general MIME messages*/
sealed trait Mime{
  val version = "1.0"
  val headers: Map[String, String] //headers not including MIME-Version

  def allHeaders = headers.updated("MIME-Version", version)
  def getMimeHeaders: Seq[String] = {
    allHeaders map {
      case (k, v) => "%s: %s".format(k, v)
    }
  }.toSeq.sortWith((s1, s2) => s1 == ("MIME-Version: %s" format version))


  /**
   * Returns the body of the message that should be sent after headers.
   * Meant to be used mainly for test purposes.
   */
  def message: String

  /**
   * The size of the whole message in bytes.
   */
  def size: Int = {
    val headersSize = getMimeHeaders map { _.length } reduceLeft { _ + _ }
    val contentSize = message.length
    headersSize + contentSize
  }

  def contentTransferEncoding: String = headers.getOrElse("Content-Transfer-Encoding", TransferEncoding.default.value)
  def contentDisposition: String = headers.getOrElse("Content-Disposition", ContentDisposition.default.value)
  def contentType: String = headers.getOrElse("Content-Type", ContentType.default.value)

  def addHeader(key: String, value: String): Mime
  def addHeader(header: MimeHeader): Mime
  def addHeaders(newHeaders: Map[String, String]): Mime
  def addHeaders(newHeaders: Seq[MimeHeader]): Mime
  }

object Mime {
  val empty = MimePart.empty

  private def textContent(text: String, subtype: String, enc: Charset) ={
    val basic = MimePart(text.getBytes(enc))
    val withHeaders =
      if (enc != CharsetUtil.US_ASCII) {
        basic
          .setContentType(ContentType("text", subtype, Map("charset" -> enc.displayName())))
          .setContentTransferEncoding(TransferEncoding.EightBit)
      }
      else basic.setContentType(ContentType("text", subtype))

    withHeaders
  }
  def plainText(text: String, enc: Charset): MimePart = textContent(text, "plain", enc)

  def plainText(text: String, encName: String): MimePart = plainText(text, Charset.forName(encName))

  def plainText(text: String): MimePart = plainText(text, Charset.forName("US-ASCII"))

  def html(text: String, enc: Charset): MimePart = textContent(text, "html", enc)

  def html(text: String, encName: String): MimePart = html(text, Charset.forName(encName))

  def html(text: String): MimePart = html(text, Charset.forName("US-ASCII"))

  def fromFile(path: String) = {
    val file = new File(path)
    val contents = Files.readBytes(file)
    val probe = new javax.activation.MimetypesFileTypeMap().getContentType(file)//java.nio.file.Files.probeContentType(javaPath)
    val ct = if (probe == null) ContentType.default
      else ContentType parse probe

    MimePart(contents, Map("Content-Type" -> ct.value))
  }
}

/*A simple MIME message with some content*/
case class MimePart(content: Array[Byte], headers: Map[String, String] = Map.empty) extends Mime {
  def message = new String(content, "US-ASCII")
  def addHeader(key: String, value: String) = copy(headers = this.headers.updated(key, value))
  def addHeader(header: MimeHeader) = copy(headers = this.headers.updated(header.name, header.value))
  def addHeaders(newHeaders: Seq[MimeHeader]) = copy(headers = this.headers ++ newHeaders.map(h => (h.name, h.value)))
  def addHeaders(newHeaders: Map[String, String]) = copy(headers = this.headers ++ newHeaders)

  def setContentType(ct: ContentType) = addHeader(ct)
  def setCharset(charset: String) = {
    val ct = ContentType parse this.contentType

    if (ct.mediatype == "text") setContentType(ct.copy(params = ct.params.updated("charset", charset)))
    else this
  }
  def setContentTransferEncoding(te: TransferEncoding) = addHeader(te)
  def setContentDisposition(cd: ContentDisposition) = addHeader(cd)
}

object MimePart {
  val empty = MimePart(Array.empty)
}

/*A multipart MIME message*/

case class MimeMultipart(parts: Seq[MimePart], headers: Map[String, String] = Map.empty)(implicit val boundary: String = "d5f6s8asdkfh3")
  extends Mime {
  private val multiContentType = ContentType("multipart", "mixed", Map("boundary" -> boundary))
  val delimiter = "--%s" format boundary
  val closingDelimiter = "--%s--" format boundary

  override def allHeaders = super.allHeaders.updated("Content-Type", multiContentType.value)

  def addHeader(key: String, value: String) = copy(headers = this.headers.updated(key, value))
  def addHeader(header: MimeHeader) = copy(headers = this.headers.updated(header.name, header.value))
  def addHeaders(newHeaders: Seq[MimeHeader]) = copy(headers = this.headers ++ newHeaders.map(h => (h.name, h.value)))
  def addHeaders(newHeaders: Map[String, String]) = copy(headers = this.headers ++ newHeaders)
  
  def message: String = {
    val partHeaders = parts map {_.getMimeHeaders.filter(!_.startsWith("MIME-Version")) mkString "\r\n" }
    val partMessages = parts map { _.message }
    val partStrings = (partHeaders zip partMessages) map { case (h, m) => "\r\n%s\r\n\r\n%s\r\n".format(h, m)}
    partStrings.mkString(delimiter, delimiter, closingDelimiter)
  }

  def addPart(part: MimePart) = copy(parts = this.parts :+ part)
  def addParts(newparts: Seq[MimePart]) = copy(parts = this.parts ++ newparts)
  def + (part: MimePart) = addPart(part)

  def setBoundary(bnd: String) = copy()(bnd)
}

object MimeMultipart {
  val empty = MimeMultipart(Seq.empty)
  def wrap(part: MimePart): MimeMultipart = MimeMultipart(Seq(part))
}