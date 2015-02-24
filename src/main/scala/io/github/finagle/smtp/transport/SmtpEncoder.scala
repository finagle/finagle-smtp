package io.github.finagle.smtp.transport

import com.twitter.util.NonFatal
import io.github.finagle.smtp.Request
import org.jboss.netty.channel._

/**
 * Encodes a Request into a ChannelBuffer.
 */
class SmtpEncoder extends SimpleChannelDownstreamHandler {
  override def writeRequested(ctx: ChannelHandlerContext, evt: MessageEvent): Unit =
    evt.getMessage match {
      case req: Request =>
        try {
          Channels.write(ctx, evt.getFuture, req.toChannelBuffer(), evt.getRemoteAddress)
        } catch {
          case NonFatal(e) =>
            evt.getFuture.setFailure(new ChannelException(e.getMessage))
        }

      case unknown =>
        evt.getFuture.setFailure(new ChannelException(
          "Unsupported request type %s".format(unknown.getClass.getName)))
    }
}
