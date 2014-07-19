package com.twitter.finagle.smtp

import java.net.InetAddress
import java.nio.charset.Charset
import org.jboss.netty.util.CharsetUtil

trait Request

class TextRequest(val cmd: String, val extensions: Map[String, String] = Map.empty) extends Request
class MimeRequest(val mime: MimePart) extends Request

sealed trait BodyEncoding
object BodyEncoding {
  case object SevenBit extends BodyEncoding { override def toString = "7BIT"}
  case object EightBit extends BodyEncoding { override def toString = "8BITMIME"}
  case object Binary   extends BodyEncoding { override def toString = "BINARYMIME"}
}

object Request {
  val Hello = new TextRequest("EHLO " + InetAddress.getLocalHost.getHostName) //Get information about the server
  val SimpleHello = new TextRequest("HELO " + InetAddress.getLocalHost.getHostName)
  val Quit = new TextRequest("QUIT")  //Close connection
  val Reset = new TextRequest("RSET") //Reset mailing session, returning to initial state
  val Noop = new TextRequest("NOOP")  //Wait an OK response from server
  val BeginData = new TextRequest("DATA") //Indicate that data is sent

  case class NewMailingSession(sender: MailingAddress, ext: Map[String, String] = Map.empty)
    extends TextRequest("MAIL FROM: <%s>".format(sender.mailbox), ext) {
    def messageSize(size: Int) = copy(ext = this.ext.updated("SIZE", size.toString))
    def bodyEncoding(be: BodyEncoding) = copy(ext = this.ext.updated("BODY", be.toString))
  }
  case class AddRecipient(rcpt: MailingAddress) extends TextRequest("RCPT TO: <" + rcpt.mailbox + ">")

  case class BeginDataChunk(size: Int) extends TextRequest("BDAT %d" format size) //for CHUNKING
  case class BeginLastDataChunk(size: Int) extends TextRequest("BDAT %d LAST" format size) //for CHUNKING; only for the last part

  case class TextData(text: Seq[String], enc: Charset = CharsetUtil.US_ASCII) extends TextRequest(text.mkString("\r\n"))
  case class MimeData(data: MimePart) extends MimeRequest(data)

  case class VerifyAddress(address: MailingAddress) extends TextRequest("VRFY " + address.mailbox)
  case class ExpandMailingList(list: MailingAddress) extends TextRequest("EXPN " + list.mailbox)
}
