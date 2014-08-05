package com.twitter.finagle.smtp.extension

import com.twitter.finagle.smtp.{TextRequest, MailingAddress}
import org.jboss.netty.buffer.ChannelBuffers
import org.jboss.netty.util.CharsetUtil

class ExtendedRequest(cmd: String, val extensions: Map[String,String]) extends TextRequest(cmd) {
  override def toChannelBuffer = {
    val params = extensions map {case (k, v) => "%s=%s".format(k, v)}
    val fullCmd = cmd + params.mkString(" " , " ", "")
    ChannelBuffers.copiedBuffer(fullCmd + "\r\n", CharsetUtil.US_ASCII)
  }
}

sealed trait BodyEncoding
object BodyEncoding {
  case object SevenBit extends BodyEncoding { override def toString = "7BIT"}
  case object EightBit extends BodyEncoding { override def toString = "8BITMIME"}
  case object Binary   extends BodyEncoding { override def toString = "BINARYMIME"}
}

case class ExtendedMailingSession(sender: MailingAddress, ext: Map[String, String] = Map.empty)
  extends ExtendedRequest("MAIL FROM: <%s>" format sender.mailbox, ext){
  def messageSize(size: Int) = copy(ext = this.ext.updated("SIZE", size.toString))
  def bodyEncoding(be: BodyEncoding) = copy(ext = this.ext.updated("BODY", be.toString))
  def authenticatedSender(mb: MailingAddress) = copy(ext = this.ext.updated("AUTH", "<%s>" format mb.mailbox))
}
