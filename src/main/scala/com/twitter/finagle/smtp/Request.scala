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

class ExtendedRequest(cmd: String, val extensions: Map[String,String]) extends TextRequest(cmd) {
  override def toChannelBuffer = {
    val params = extensions map {case (k, v) => "%s=%s".format(k, v)}
    val fullCmd = cmd + params.mkString(" " , " ", "")
    ChannelBuffers.copiedBuffer(fullCmd + "\r\n", CharsetUtil.US_ASCII)
  }
}

class MimeRequest(val mime: MimePart) extends Request {
  def toChannelBuffer = {
    val headerBytes = mime.getMimeHeaders.mkString("", "\r\n", "\r\n").getBytes(CharsetUtil.US_ASCII)
    ChannelBuffers.copiedBuffer(headerBytes, mime.content)
  }
}

case class RequestGroup(reqs: Seq[Request]) extends Request {
  // The contents (subrequests) of this request are supposed to be sent
  // separately, so the request itself cannot be sent
  def toChannelBuffer = ChannelBuffers.wrappedBuffer(Array[Byte]())
}

private[smtp] case class GroupPart(req: Request) extends Request {
  def toChannelBuffer = req.toChannelBuffer
}

sealed trait BodyEncoding
object BodyEncoding {
  case object SevenBit extends BodyEncoding { override def toString = "7BIT"}
  case object EightBit extends BodyEncoding { override def toString = "8BITMIME"}
  case object Binary   extends BodyEncoding { override def toString = "BINARYMIME"}
}

object Request {
  val Hello = new HelloRequest("EHLO") //Get information about the server
  val SimpleHello = new HelloRequest("HELO")
  val Quit = new TextRequest("QUIT")  //Close connection
  val Reset = new TextRequest("RSET") //Reset mailing session, returning to initial state
  val Noop = new TextRequest("NOOP")  //Wait an OK response from server
  val BeginData = new BeginDataRequest("DATA") //Indicate that data is sent

  case class NewMailingSession(sender: MailingAddress, ext: Map[String, String] = Map.empty)
    extends ExtendedRequest("MAIL FROM: <%s>".format(sender.mailbox), ext){
    def messageSize(size: Int) = copy(ext = this.ext.updated("SIZE", size.toString))
    def bodyEncoding(be: BodyEncoding) = copy(ext = this.ext.updated("BODY", be.toString))
  }
  case class AddRecipient(rcpt: MailingAddress) extends TextRequest("RCPT TO: <" + rcpt.mailbox + ">")

  case class BeginDataChunk(size: Int) extends BeginDataRequest("BDAT %d" format size) //for CHUNKING
  case class BeginLastDataChunk(size: Int) extends BeginDataRequest("BDAT %d LAST" format size) //for CHUNKING; only for the last part

  case class TextData(text: Seq[String], enc: Charset = CharsetUtil.US_ASCII) extends TextRequest(text.mkString("\r\n"))
  case class MimeData(data: MimePart) extends MimeRequest(data)

  case class VerifyAddress(address: MailingAddress) extends TextRequest("VRFY " + address.mailbox)
  case class ExpandMailingList(list: MailingAddress) extends TextRequest("EXPN " + list.mailbox)
}
