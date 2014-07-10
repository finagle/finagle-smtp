package com.twitter.finagle.smtp.transport

import com.twitter.finagle.smtp.Request
import com.twitter.util.NonFatal
import org.jboss.netty.channel._
import org.jboss.netty.buffer.ChannelBuffers
import org.jboss.netty.util.CharsetUtil
import com.twitter.util.NonFatal
import com.twitter.finagle.smtp.{TextRequest, Request}

/**
 * Encodes a Request into a ChannelBuffer.
 */
class SmtpEncoder extends SimpleChannelDownstreamHandler {
  override def writeRequested(ctx: ChannelHandlerContext, evt: MessageEvent): Unit =
    evt.getMessage match {
      case req: TextRequest =>
        try {
          val buf = ChannelBuffers.copiedBuffer(req.cmd + "\r\n", CharsetUtil.US_ASCII)
          Channels.write(ctx, evt.getFuture, buf, evt.getRemoteAddress)
        } catch {
          case NonFatal(e) =>
            evt.getFuture.setFailure(new ChannelException(e.getMessage))
        }

      case Request.MimeData(mime) =>
        try {
          val headersbuf = ChannelBuffers.copiedBuffer(mime.getMimeHeaders.mkString("", "\r\n", "\r\n"), CharsetUtil.US_ASCII)
          val contentbuf = ChannelBuffers.copiedBuffer(mime.content)
          Channels.write(ctx, evt.getFuture, headersbuf, evt.getRemoteAddress)
          Channels.write(ctx, evt.getFuture, contentbuf, evt.getRemoteAddress)
        } catch {
          case NonFatal(e) =>
            evt.getFuture.setFailure(new ChannelException(e.getMessage))
        }

      case unknown =>
        evt.getFuture.setFailure(new ChannelException(
          "Unsupported request type %s".format(unknown.getClass.getName)))
    }
}
