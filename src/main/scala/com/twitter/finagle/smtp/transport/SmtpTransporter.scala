package io.github.finagle.smtp.transport

import com.twitter.finagle.netty3.Netty3Transporter
import io.github.finagle.smtp.Request
import io.github.finagle.smtp.reply.UnspecifiedReply
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

