package com.twitter.finagle.smtp

import java.nio.charset.Charset

/*A trait for general MIME messages*/
sealed trait Mime {
  val version = "1.0"
  val headers: Map[String, String] //headers not including MIME-Version
  def allHeaders = headers.updated("MIME-Version", version)  
  def getMimeHeaders: Seq[String] = {
    allHeaders map {
      case (k, v) => "%s: %s".format(k, v)
    }
  }.toSeq


  /*Returns the body of the message that should be sent after headers*/
  def message: String

  def contentTransferEncoding = headers.getOrElse("Content-Transfer-Encoding", TransferEncoding.default)
  def contentDisposition = headers.getOrElse("Content-Disposition", ContentDisposition.default)

  def addHeader(key: String, value: String): Mime
  def addHeaders(newHeaders: Map[String, String]): Mime
  }

//TODO: various shortcuts for common media types

object Mime {
  def plainText(text: String, enc: Charset) = MimePart(text.getBytes(enc),
    Map { "Content-Type" -> ("text/plain;charset="+enc.displayName()) }
  )
  def plainText(text: String) = MimePart(text.getBytes(Charset.forName("US-ASCII")),
    Map { "Content-Type" -> ("text/plain") }
  )
}

object MimePart {
  val empty = MimePart(Array.empty)
}

case class MimePart(content: Array[Byte], headers: Map[String, String] = Map.empty) extends Mime {
  def message = String.copyValueOf(content.map(_.toChar))

  def addHeader(key: String, value: String) = copy(content, headers.updated(key, value))
  def addHeaders(newHeaders: Map[String, String]) = copy(content, headers ++ newHeaders)

  def setContentType(ct: ContentType) = addHeader("Content-Type", ct.toString)
  def setTransferEncoding(te: String) = addHeader("Content-Transfer-Encoding", te)
  def setContentDisposition(cd: ContentDisposition) = addHeader("Content-Disposition", cd.toString)
}

case class MimeMultipart(parts: Seq[MimePart], headers: Map[String, String])(implicit val boundary: String = "boundary")
  extends Mime {
  val multiContentType = ContentType("multipart", "mixed", Map("boundary" -> boundary))
  val richBoundary = "--%s--".format(boundary)

  override def allHeaders = super.allHeaders.updated("Content-Type", multiContentType.toString)
  def addHeader(key: String, value: String) = copy(headers = this.headers.updated(key, value))
  def addHeaders(newHeaders: Map[String, String]) = copy(headers = this.headers ++ newHeaders)
  
  def message: String = {
    val partHeaders = parts map {_.getMimeHeaders mkString "\r\n" }
    val partMessages = parts map { _.message }
    val partStrings = (partHeaders zip partMessages) map { case (h, m) => "\r\n%s\r\n%s\r\n".format(h, m)}
    partStrings.mkString(richBoundary, richBoundary, richBoundary)
  }

  def addPart(part: MimePart) = copy(parts = this.parts :+ part)
  def + (part: MimePart) = addPart(part)

  def setBoundary(bnd: String) = copy()(bnd)
}

object MimeMultipart {
  val empty = MimeMultipart(Seq.empty, Map.empty)
  def apply(part: MimePart): MimeMultipart = MimeMultipart(Seq(part), Map.empty)
}

object TransferEncoding {
  val sevenBit = "7bit"
  val eightBit = "8bit"
  val quotedPrintable = "quoted-printable"
  val binary = "binary"
  val base64 = "base64"

  val default = sevenBit
}

trait HeaderWithParams {
  val params: Map[String, String]
  def paramsString = {
    if (params.isEmpty) ""
    else params
        .map {case (k: String, v: String) => k + "=" + v}
        .mkString(";", ", ", "")
  }
}

case class ContentType(mediatype: String, subtype: String, params: Map[String, String] = Map.empty)
 extends HeaderWithParams {
  override def toString = mediatype + "/" + subtype + paramsString
}

object ContentDisposition {
  val Inline = "inline"
  val Attachment = "attachment"

  val default = ContentDisposition(Inline)
}

case class ContentDisposition(pres: String, params: Map[String, String] = Map.empty)
 extends HeaderWithParams {
  override def toString = pres + paramsString
}