package com.twitter.finagle.smtp

import java.io.File
import java.nio.charset.Charset

import org.jboss.netty.util.CharsetUtil

/**
 * A trait for general MIME messages.
 */
sealed trait Mime{
  val version = "1.0"
  /**
   * Headers not including MIME-Version.
   */
  val headers: Map[String, String]

  /**
   * Gets all MIME headers of the message (including MIME-Version)
   * in the form of a Map.
   */
  def allHeaders: Map[String, String] = headers.updated("MIME-Version", version)

  /**
   * Gets string representations of all MIME headers of the message,
   * including MIME-Version, ensuring that the latter is the first header.
   */
  def getMimeHeaders: Seq[String] = {
    allHeaders map {
      case (k, v) => "%s: %s".format(k, v)
    }
  }.toSeq.sortWith((s1, s2) => s1 == ("MIME-Version: %s" format version))


  /**
   * Returns the body of the message that should be sent after headers
   * as a String. Meant to be used mainly for test purposes.
   */
  def message(): String

  /**
   * The size of the whole message in bytes.
   */
  def size: Int = {
    val headersSize = getMimeHeaders map { _.length } reduceLeft { _ + _ }
    val contentSize = message().length
    headersSize + contentSize
  }

  /**
   * Gets content transfer encoding of the message.
   */
  def contentTransferEncoding: String = headers.getOrElse("Content-Transfer-Encoding", TransferEncoding.default.value)

  /**
   * Gets content disposition of the message.
   */
  def contentDisposition: String = headers.getOrElse("Content-Disposition", ContentDisposition.default.value)

  /**
   * Gets content type of the message.
   */
  def contentType: String = headers.getOrElse("Content-Type", ContentType.default.value)


  /**
   * Adds a header to the message.
   *
   * @param key Header name
   * @param value Header value
   */
  def addHeader(key: String, value: String): Mime

  /**
   * Adds a header to the message.
   *
   * @param header [[com.twitter.finagle.smtp.MimeHeader]] representing the header.
   */
  def addHeader(header: MimeHeader): Mime

  /**
   * Adds several headers to the message.
   *
   * @param newHeaders Map from header name to header value
   */
  def addHeaders(newHeaders: Map[String, String]): Mime

  /**
   * Adds several headers to the message.
   *
   * @param newHeaders Sequence of [[com.twitter.finagle.smtp.MimeHeader MimeHeaders]]
   */
  def addHeaders(newHeaders: Seq[MimeHeader]): Mime
  }

object MimeTypes {
  def withTypes(types: Seq[String]) = default ++ types

  val default = Seq (
    "text/html html htm",
    "text/plain txt text",
    "text/richtext rtf",
    "image/gif gif GIF",
    "image/png png",
    "image/jpeg jpeg jpg jpe JPG",
    "image/tiff tiff tif",
    "audio/midi midi mid",
    "audio/aifc aifc",
    "audio/aiff aif aiff",
    "audio/mpeg mpeg mpg",
    "audio/wav wav",
    "video/mpeg mpeg mpg mpe",
    "video/quicktime qt mov",
    "video/avi avi",
    "application/zip zip"
  )
}

object Mime {
  val empty = MimePart.empty

  protected val mimeTypeMap = new javax.activation.MimetypesFileTypeMap
  MimeTypes.default foreach mimeTypeMap.addMimeTypes

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
  
  /**
   * Creates a plain text [[com.twitter.finagle.smtp.MimePart]] encoded in a given charset.
   * 
   * @param text The text of the part
   * @param enc The charset in which the text should be encoded
   */
  def plainText(text: String, enc: Charset): MimePart = textContent(text, "plain", enc)

  /**
   * Creates a plain text [[com.twitter.finagle.smtp.MimePart]] encoded in a given charset.
   *
   * @param text The text of the part
   * @param encName The name of the charset in which the text should be encoded
   */
  def plainText(text: String, encName: String): MimePart = plainText(text, Charset.forName(encName))

  /**
   * Creates a plain text [[com.twitter.finagle.smtp.MimePart]] encoded in US-ASCII charset.
   *
   * @param text The text of the part
   */
  def plainText(text: String): MimePart = plainText(text, Charset.forName("US-ASCII"))

  /**
   * Creates an HTML [[com.twitter.finagle.smtp.MimePart]] encoded in given charset
   *
   * @param text The HTML text
   * @param enc The charset in which the text should be encoded
   */
  def html(text: String, enc: Charset): MimePart = textContent(text, "html", enc)

  /**
   * Creates an HTML [[com.twitter.finagle.smtp.MimePart]] encoded in given charset
   *
   * @param text The HTML text
   * @param encName The name of the charset in which the text should be encoded
   */
  def html(text: String, encName: String): MimePart = html(text, Charset.forName(encName))

  /**
   * Creates an HTML [[com.twitter.finagle.smtp.MimePart]] encoded in given charset
   *
   * @param text The HTML text
   */
  def html(text: String): MimePart = html(text, Charset.forName("US-ASCII"))

  /**
   * Creates a [[com.twitter.finagle.smtp.MimePart]] with contents from given file.
   * 
   * @param path The path to the file
   */
  def fromFile(path: String): MimePart = {
    val file = new File(path)
    val contents = com.twitter.io.Files.readBytes(file)
    val probe = mimeTypeMap.getContentType(path)
    val ct = if (probe == "application/octet-stream") ContentType.default
      else ContentType parse probe

    MimePart(contents, Map("Content-Type" -> ct.value))
  }
}

/**
 * A simple MIME message with some content.
 */
case class MimePart(content: Array[Byte], headers: Map[String, String] = Map.empty) extends Mime {
  def message(): String = new String(content, "US-ASCII")

  def addHeader(key: String, value: String): MimePart =
    copy(headers = this.headers.updated(key, value))

  def addHeader(header: MimeHeader): MimePart =
    copy(headers = this.headers.updated(header.name, header.value))

  def addHeaders(newHeaders: Seq[MimeHeader]): MimePart =
    copy(headers = this.headers ++ newHeaders.map(h => (h.name, h.value)))

  def addHeaders(newHeaders: Map[String, String]): MimePart =
    copy(headers = this.headers ++ newHeaders)

  /**
   * Sets the ''Content-Type'' header of the message.
   */
  def setContentType(ct: ContentType): MimePart = addHeader(ct)

  /**
   * Sets the charset of the message if its content type is text.
   */
  def setCharset(charset: String): MimePart = {
    val ct = ContentType parse this.contentType

    if (ct.mediatype == "text")
      setContentType(ct.copy(params = ct.params.updated("charset", charset)))
    else this
  }

  /**
   * Sets the ''Content-Transfer-Encoding'' header of the message.
   */
  def setContentTransferEncoding(te: TransferEncoding): MimePart = addHeader(te)

  /**
   * Sets the ''Content-Disposition'' header of the message.
   */
  def setContentDisposition(cd: ContentDisposition): MimePart = addHeader(cd)
}

object MimePart {

  /**
   * An empty [[com.twitter.finagle.smtp.MimePart]].
   */
  val empty: MimePart = MimePart(Array.empty)
}

/**
 * A multipart MIME message.
 *
 * @param parts The parts of the message
 * @param headers The headers of this whole multipart message
 * @param boundary The boundary that will be used to separate the parts and
 *                 indicate the end of the message.
 */
case class MimeMultipart(parts: Seq[MimePart], headers: Map[String, String] = Map.empty)
  (implicit val boundary: String = "d5f6s8asdkfh3") extends Mime {

  // The content type of the whole multipart message
  private val multiContentType = ContentType("multipart", "mixed", Map("boundary" -> boundary))

  /**
   * The delimiter used to separate the parts
   */
  def delimiter: String = "--%s" format boundary

  /**
   * The delimiter used to indicate the end of the message
   */
  def closingDelimiter: String = "--%s--" format boundary

  override def allHeaders: Map[String, String] = super.allHeaders.updated("Content-Type", multiContentType.value)

  def addHeader(key: String, value: String): MimeMultipart =
    copy(headers = this.headers.updated(key, value))

  def addHeader(header: MimeHeader): MimeMultipart =
    copy(headers = this.headers.updated(header.name, header.value))

  def addHeaders(newHeaders: Seq[MimeHeader]): MimeMultipart =
    copy(headers = this.headers ++ newHeaders.map(h => (h.name, h.value)))

  def addHeaders(newHeaders: Map[String, String]): MimeMultipart =
    copy(headers = this.headers ++ newHeaders)
  
  def message(): String = {
    val partHeaders = parts map {_.getMimeHeaders.filter(!_.startsWith("MIME-Version")) mkString "\r\n" }
    val partMessages = parts map { _.message() }
    val partStrings = (partHeaders zip partMessages) map { case (h, m) => "\r\n%s\r\n\r\n%s\r\n".format(h, m)}
    partStrings.mkString(delimiter, delimiter, closingDelimiter)
  }

  /**
   * Adds given [[com.twitter.finagle.smtp.MimePart]] to the message.
   */
  def addPart(part: MimePart) = copy(parts = this.parts :+ part)

  /**
   * Adds given sequence of [[com.twitter.finagle.smtp.MimePart]] to the message.
   */
  def addParts(newparts: Seq[MimePart]) = copy(parts = this.parts ++ newparts)

  /**
   * Syntactic sugar for [[com.twitter.finagle.smtp.MimeMultipart.addPart()]]
   */
  def + (part: MimePart) = addPart(part)

  /**
   * Sets the boundary of the message.
   */
  def setBoundary(bnd: String) = copy()(bnd)
}

object MimeMultipart {
  /**
   * An empty [[com.twitter.finagle.smtp.MimeMultipart]].
   */
  val empty: MimeMultipart = MimeMultipart(Seq.empty)

  /**
   * Creates a [[com.twitter.finagle.smtp.MimeMultipart]] containing
   * only given [[com.twitter.finagle.smtp.MimePart]].
   */
  def wrap(part: MimePart): MimeMultipart = MimeMultipart(Seq(part))
}