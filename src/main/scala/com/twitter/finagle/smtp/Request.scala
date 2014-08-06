package com.twitter.finagle.smtp

import java.net.InetAddress

/**
 * Represents SMTP request.
 */
trait Request

class TextRequest(val cmd: String) extends Request
class MimeRequest(val mime: MimePart) extends Request

/**
 * Contains subclasses and instances of known SMTP requests.
 */
object Request {

  /** Gets information about the server */
  val Hello = new TextRequest("EHLO " + InetAddress.getLocalHost.getHostName) //Get information about the server

  /** Tells the server to close connection */
  val Quit = new TextRequest("QUIT")

  /** Resets session */
  val Reset = new TextRequest("RSET")

  /** Requests OK reply from the server */
  val Noop = new TextRequest("NOOP")

  /** Indicates that email body is going to be sent */
  val BeginData = new TextRequest("DATA")

  /** Starts mailing session and indicates sender mailbox */
  case class AddSender(addr: MailingAddress) extends TextRequest("MAIL FROM: <" + addr.mailbox + ">")

  /** Adds a new recipient mailbox */
  case class AddRecipient(rcpt: MailingAddress) extends TextRequest("RCPT TO: <" + rcpt.mailbox + ">")

  case class TextData(text: Seq[String]) extends TextRequest(text.mkString("\r\n"))
  case class MimeData(data: MimePart) extends MimeRequest(data)

  /** Requests the name of the user with given mailbox, if found */
  case class VerifyAddress(address: MailingAddress) extends TextRequest("VRFY " + address.mailbox)

  /** Requests mailboxes included in a mailing list */
  case class ExpandMailingList(list: MailingAddress) extends TextRequest("EXPN " + list.mailbox)

}
