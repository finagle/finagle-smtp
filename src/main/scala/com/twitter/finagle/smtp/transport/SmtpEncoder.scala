package com.twitter.finagle.smtp.transport

import com.twitter.finagle.smtp.Request
import com.twitter.util.NonFatal
import org.jboss.netty.channel._

/**
 * Encodes a Request into a ChannelBuffer.
 */
class SmtpEncoder extends SimpleChannelDownstreamHandler {
  override def writeRequested(ctx: ChannelHandlerContext, evt: MessageEvent) =
    evt.getMessage match {
      case req: Request =>
        try {
          Channels.write(ctx, evt.getFuture, req.toChannelBuffer, evt.getRemoteAddress)
        } catch {
          case NonFatal(e) =>
            evt.getFuture.setFailure(new ChannelException(e.getMessage))
        }

      case unknown =>
        evt.getFuture.setFailure(new ChannelException(
          "Unsupported request type %s".format(unknown.getClass.getName)))
    }
}
