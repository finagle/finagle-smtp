package io.github.finagle.smtp.transport

import io.github.finagle.smtp.{InvalidReply, NonTerminalLine, UnspecifiedReply}
import org.jboss.netty.buffer.ChannelBuffer
import org.jboss.netty.channel.{Channel, ChannelHandlerContext}
import org.jboss.netty.handler.codec.frame.LineBasedFrameDecoder
import org.jboss.netty.util.CharsetUtil

/**
 * Decodes SMTP replies from lines ending with <CRLF>.
 * An SMTP reply is a three-digit code followed by
 * space and an optional informational string.
 *
 * If the three-digit code is followed by a hyphen,
 * treats the line as a part of a multiline reply.
 */
class SmtpDecoder extends LineBasedFrameDecoder(512) {

  private def getCode(first: Char, second: Char, third: Char): Int =
    "%c%c%c".format(first, second, third).toInt

  private def getInfo(info: Array[Char]) = new String(info)

  private def mkReply(first: Char, second: Char, third: Char, delim: Option[Char],
    inf: Array[Char]): Option[UnspecifiedReply] = {
    if (first.isDigit && second.isDigit && third.isDigit)
      delim match {
        case Some(' ') => Some(
          new UnspecifiedReply {
            val code = getCode(first, second, third)
            val info = getInfo(inf)
          })

        case Some('-') => Some(NonTerminalLine(getCode(first, second, third), getInfo(inf)))

        case None => Some(
          new UnspecifiedReply {
            val code = getCode(first, second, third)
            val info = ""
          }
        )

        case _ => None
      }

    else None
  }

  override def decode(ctx: ChannelHandlerContext, channel: Channel,
    msg: ChannelBuffer): UnspecifiedReply = {
      Option(super.decode(ctx, channel, msg)) match {
      case None => null
      case Some(buf: ChannelBuffer) =>
        val repString = buf.toString(CharsetUtil.US_ASCII)
        val repOption: Option[UnspecifiedReply] = repString.toList match {
          // in case there is only code with no delimiter
          case List(first, second, third) => mkReply(first, second, third, None, Array.empty)

          case first::second::third::delim::info => mkReply(first, second, third, Some(delim), info.toArray)

          case _ => None
        }

        repOption match {
          case Some(reply) => reply
          case None => InvalidReply(repString)
        }

      }
  }
}
