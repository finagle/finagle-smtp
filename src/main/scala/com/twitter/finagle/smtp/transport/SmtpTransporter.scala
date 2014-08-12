package com.twitter.finagle.smtp.transport

import com.twitter.finagle.netty3.Netty3Transporter
import com.twitter.finagle.smtp.{UnspecifiedReply, Request}
import org.jboss.netty.channel._

object SmtpPipeline extends ChannelPipelineFactory {
  def getPipeline = {
    val pipeline = Channels.pipeline()
    pipeline.addLast("smtpEncode", new SmtpEncoder)
    pipeline.addLast("smtpDecode", new SmtpDecoder)
    pipeline
  }
}

object SmtpTransporter extends Netty3Transporter[Request, UnspecifiedReply](
  name = "SmtpTransporter",
  pipelineFactory = SmtpPipeline)

