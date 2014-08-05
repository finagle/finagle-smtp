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
