package com.twitter.finagle.smtp

import java.net.InetAddress
import java.nio.charset.Charset
import org.jboss.netty.util.CharsetUtil

trait Request

class TextRequest(val cmd: String) extends Request
class MimeRequest(val mime: MimePart) extends Request

/**
 * Contains subclasses and instances of known SMTP requests.
 */
object Request {
  val Hello = new TextRequest("EHLO " + InetAddress.getLocalHost.getHostName) //Get information about the server
  val SimpleHello = new TextRequest("HELO " + InetAddress.getLocalHost.getHostName)
  val Quit = new TextRequest("QUIT")  //Close connection
  val Reset = new TextRequest("RSET") //Reset mailing session, returning to initial state
  val Noop = new TextRequest("NOOP")  //Wait an OK response from server
  val BeginData = new TextRequest("DATA") //Indicate that data is sent

  case class NewMailingSession(sender: MailingAddress, msgSize: Int)
    extends TextRequest("MAIL FROM: <%s> SIZE %d".format(sender.mailbox, msgSize))
  case class AddSender(addr: MailingAddress) extends TextRequest("MAIL FROM: <" + addr.mailbox + ">")
  case class AddRecipient(rcpt: MailingAddress) extends TextRequest("RCPT TO: <" + rcpt.mailbox + ">")

  case class TextData(text: Seq[String], enc: Charset = CharsetUtil.US_ASCII) extends TextRequest(text.mkString("\r\n"))
  case class MimeData(data: MimePart) extends MimeRequest(data)

  case class VerifyAddress(address: MailingAddress) extends TextRequest("VRFY " + address.mailbox)
  case class ExpandMailingList(list: MailingAddress) extends TextRequest("EXPN " + list.mailbox)
}
