package com.twitter.finagle.smtp

import java.net.InetAddress
import java.nio.charset.Charset

import org.jboss.netty.buffer.{ChannelBuffer, ChannelBuffers}
import org.jboss.netty.util.CharsetUtil

trait Request {
  def toChannelBuffer: ChannelBuffer
}

class TextRequest(val cmd: String) extends Request {
  def toChannelBuffer = ChannelBuffers.copiedBuffer(cmd + "\r\n", CharsetUtil.US_ASCII)
}

private[smtp] class HelloRequest(keyword: String) extends TextRequest("%s %s".format(keyword, InetAddress.getLocalHost.getHostName))
private[smtp] class BeginDataRequest(cmd: String) extends TextRequest(cmd)

class MimeRequest(val mime: MimePart) extends Request {
  def toChannelBuffer = {
    val headerBytes = mime.getMimeHeaders.mkString("", "\r\n", "\r\n").getBytes(CharsetUtil.US_ASCII)
    ChannelBuffers.copiedBuffer(headerBytes, mime.content)
  }
}

/**
 * Contains subclasses and instances of known SMTP requests.
 */
object Request {
  /** Gets information about the server */
  val Hello = new HelloRequest("EHLO")
  val SimpleHello = new HelloRequest("HELO")

  /** Tells the server to close connection */
  val Quit = new TextRequest("QUIT")

  /** Resets session */
  val Reset = new TextRequest("RSET")

  /** Requests OK reply from the server */
  val Noop = new TextRequest("NOOP")

  /** Indicates that email body is going to be sent */
  val BeginData = new BeginDataRequest("DATA")

  /** Starts mailing session and indicates sender mailbox */
  case class NewMailingSession(sender: MailingAddress) extends TextRequest("MAIL FROM: <%s>".format(sender.mailbox))

  /** Adds a new recipient mailbox */
  case class AddRecipient(rcpt: MailingAddress) extends TextRequest("RCPT TO: <" + rcpt.mailbox + ">")

  case class TextData(text: Seq[String], enc: Charset = CharsetUtil.US_ASCII) extends TextRequest(text.mkString("\r\n"))
  case class MimeData(data: MimePart) extends MimeRequest(data)

  /** Requests the name of the user with given mailbox, if found */
  case class VerifyAddress(address: MailingAddress) extends TextRequest("VRFY " + address.mailbox)

  /** Requests mailboxes included in a mailing list */
  case class ExpandMailingList(list: MailingAddress) extends TextRequest("EXPN " + list.mailbox)
}
