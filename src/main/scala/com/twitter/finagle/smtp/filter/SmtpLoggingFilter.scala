package com.twitter.finagle.smtp.filter

import com.twitter.finagle.filter.{LogFormatter, LoggingFilter}
import com.twitter.finagle.smtp.{InvalidReply, Reply, Request, SmtpError}
import com.twitter.logging.Logger
import com.twitter.util.Duration
import org.jboss.netty.util.CharsetUtil

object SmtpLogFormatter extends LogFormatter[Request, Reply] {
  def format(request: Request, reply: Reply, replyTime: Duration): String = {
    val req = "Client: %s".format(request.toChannelBuffer.toString(CharsetUtil.US_ASCII))
    val rep = "Server: %d %s".format(reply.code, reply.lines.mkString("\r\n"))

    req + rep
  }

  def formatException(request: Request, throwable: Throwable, replyTime: Duration): String = {
    val req = "Client: %s".format(request.toChannelBuffer.toString(CharsetUtil.US_ASCII))
    val rep = throwable match {
      case InvalidReply(content) => "Server: invalid reply - %s\r\n" format content
      case err: SmtpError => "Server: %d %s".format(err.code, err.lines.mkString("\r\n"))
      case other => other.getStackTraceString
    }

    req + rep
  }
}

class SmtpLoggingFilter (val log: Logger) extends LoggingFilter[Request, Reply] {
  val formatter = SmtpLogFormatter
}
