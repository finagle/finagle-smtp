package com.twitter.finagle.smtp.transport

import com.twitter.finagle.smtp.{InvalidReply, UnspecifiedReply}
import org.jboss.netty.channel.{ChannelHandlerContext, Channels, MessageEvent, SimpleChannelUpstreamHandler}

class SmtpDecoder extends SimpleChannelUpstreamHandler{
  import com.twitter.finagle.smtp.transport.CodecUtil._
  override def messageReceived(ctx: ChannelHandlerContext, e: MessageEvent) = {
    val pipeline = ctx.getPipeline
    if (pipeline.get(aggregation) != null)
      pipeline.remove(aggregation)
    e.getMessage match {
      case rep: UnspecifiedReply => Channels.fireMessageReceived(ctx, rep)
      case other => Channels.fireMessageReceived(ctx, InvalidReply(other.toString))
    }
  }
}
