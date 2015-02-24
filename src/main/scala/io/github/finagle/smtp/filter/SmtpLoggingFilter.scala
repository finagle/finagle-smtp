package io.github.finagle.smtp.filter

import com.twitter.finagle.filter.{LogFormatter, LoggingFilter}
import com.twitter.logging.Logger
import com.twitter.util.Duration
import io.github.finagle.smtp.{InvalidReply, Reply, Request, SmtpError}
import org.jboss.netty.util.CharsetUtil

object SmtpLogFormatter extends LogFormatter[Request, Reply] {
  def format(request: Request, reply: Reply, replyTime: Duration): String = {
    val req = "Client: %s".format(request.toChannelBuffer().toString(CharsetUtil.US_ASCII))
    val rep =
      if (reply.isMultiline) {
        val lines = reply.lines()
        val nonTerminalLines = lines.dropRight(1) map {
          "%d-%s".format(reply.code, _)
        }
        val terminalLine = "%d %s".format(reply.code, lines.last)

        "Server:\r\n" + nonTerminalLines.mkString("\r\n") + terminalLine
      }
      else "Server: %d %s".format(reply.code, reply.info)

    req + rep
  }

  def formatException(request: Request, throwable: Throwable, replyTime: Duration): String = {
    val req = "Client: %s".format(request.toChannelBuffer().toString(CharsetUtil.US_ASCII))
    val rep = throwable match {
      case InvalidReply(content) => "Server: invalid reply - %s\r\n" format content
      case err: SmtpError => "Server: %d %s".format(err.code, err.info)
      case other => other.getStackTraceString
    }

    req + rep
  }
}

/**
 * Logs SMTP session using [[io.github.finagle.smtp.filter.SmtpLogFormatter]]
 */
case class SmtpLoggingFilter(log: Logger) extends LoggingFilter[Request, Reply] {
  val formatter = SmtpLogFormatter
}
