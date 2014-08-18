package com.twitter.finagle.smtp

import java.net.InetAddress
import java.nio.charset.Charset
import org.jboss.netty.buffer.{ChannelBuffer, ChannelBuffers}
import org.jboss.netty.util.CharsetUtil

/**
 * Represents SMTP request.
 */
trait Request {
  /**
   * Encodes the request into a ChannelBuffer
   */
  def toChannelBuffer(): ChannelBuffer
}

/**
 * A simple text request
 *
 * @param cmd The command that will be sent to server
 */
class TextRequest(val cmd: String) extends Request {
  def toChannelBuffer: ChannelBuffer =
    ChannelBuffers.copiedBuffer(cmd + "\r\n", CharsetUtil.US_ASCII)
}

private[smtp] class HelloRequest(keyword: String) extends TextRequest("%s %s".format(keyword, InetAddress.getLocalHost.getHostAddress))
private[smtp] class BeginDataRequest(cmd: String) extends TextRequest(cmd)

/**
 * The request containing a MIME message
 */
class MimeRequest(val mime: MimePart) extends Request {
  def toChannelBuffer(): ChannelBuffer = {
    val headerBytes = mime.getMimeHeaders.mkString("", "\r\n", "\r\n").getBytes(CharsetUtil.US_ASCII)
    ChannelBuffers.copiedBuffer(headerBytes, mime.content)
  }
}

object Request {
  val Hello = new HelloRequest("EHLO") //Get information about the server
  val SimpleHello = new HelloRequest("HELO")
  val Quit = new TextRequest("QUIT")  //Close connection
  val Reset = new TextRequest("RSET") //Reset mailing session, returning to initial state
  val Noop = new TextRequest("NOOP")  //Wait an OK response from server
  val BeginData = new BeginDataRequest("DATA") //Indicate that data is sent

  case class NewMailingSession(sender: MailingAddress) extends TextRequest("MAIL FROM: <%s>".format(sender.mailbox))

  case class AddRecipient(rcpt: MailingAddress) extends TextRequest("RCPT TO: <" + rcpt.mailbox + ">")

  case class TextData(text: Seq[String], enc: Charset = CharsetUtil.US_ASCII) extends TextRequest(text.mkString("\r\n"))
  case class MimeData(data: MimePart) extends MimeRequest(data)

  case class VerifyAddress(address: MailingAddress) extends TextRequest("VRFY " + address.mailbox)
  case class ExpandMailingList(list: MailingAddress) extends TextRequest("EXPN " + list.mailbox)
}
