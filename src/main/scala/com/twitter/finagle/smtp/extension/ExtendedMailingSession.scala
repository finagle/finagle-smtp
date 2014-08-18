package com.twitter.finagle.smtp.extension

import com.twitter.finagle.smtp.{MailingAddress, TextRequest}
import org.jboss.netty.buffer.ChannelBuffers
import org.jboss.netty.util.CharsetUtil

/**
 * SMTP request with additional parameters corresponding to some
 * SMTP extensions.
 */
class ExtendedRequest(cmd: String, val extensions: Map[String,String]) extends TextRequest(cmd) {
  override def toChannelBuffer = {
    val params = extensions map {case (k, v) => "%s=%s".format(k, v)}
    val fullCmd = cmd + params.mkString(" " , " ", "")
    ChannelBuffers.copiedBuffer(fullCmd + "\r\n", CharsetUtil.US_ASCII)
  }
}

/**
 * Message body encoding: 7-bit ASCII, 8-bit octets or binary data
 */
sealed trait BodyEncoding
object BodyEncoding {
  case object SevenBit extends BodyEncoding { override def toString = "7BIT"}
  case object EightBit extends BodyEncoding { override def toString = "8BITMIME"}
  case object Binary   extends BodyEncoding { override def toString = "BINARYMIME"}
}

/**
 * Extended ''MAIL FROM'' command with parameters from some SMTP extensions.
 */
case class ExtendedMailingSession(sender: MailingAddress, ext: Map[String, String] = Map.empty)
  extends ExtendedRequest("MAIL FROM: <%s>" format sender.mailbox, ext){
  private def addParam(keyword: String, value: String) =
    copy(ext = this.ext.updated(keyword, value))

  /**
   * Adds ''SIZE'' parameter to ''MAIL FROM'' command. It is used to indicate
   * the size of message to be sent, and can only be accepted and processed if
   * ''SIZE'' extension is supported.
   */
  def messageSize(size: Int): ExtendedMailingSession = addParam("SIZE", size.toString())

  /**
   * Adds ''BODY'' parameter to ''MAIL FROM'' command. It is used to indicate the encoding
   * of the message to be sent, and can only be accepted and processed if ''8BITMIME'' or
   * ''BINARYMIME'' extensions are supported.
   */
  def bodyEncoding(be: BodyEncoding) = addParam("BODY", be.toString())

  /**
   * Adds ''AUTH'' parameter to ''MAIL FROM'' command. It is used to indicate
   * authorized identity for the message to be sent.
   *
   * Note that this may not be the authenticated identity negotiated using
   * [[com.twitter.finagle.smtp.extension.auth.AuthRequest]]
   * and that one authenticated identity may send messages from arbitrary number
   * of authorized identities.
   *
   * This parameter can only be accepted and processed if ''AUTH'' extension is
   * supported. However, it can be sent without sending
   * [[com.twitter.finagle.smtp.extension.auth.AuthRequest]] first.
   */
  def authorize(mb: MailingAddress) = addParam("AUTH", "<%s>" format mb.mailbox)
}
