package io.github.finagle.smtp.transport

import com.twitter.finagle.Stack
import com.twitter.finagle.client.Transporter
import com.twitter.finagle.netty3.Netty3Transporter
import io.github.finagle.smtp.{Request, UnspecifiedReply}
import org.jboss.netty.channel._

object SmtpPipeline extends ChannelPipelineFactory {
  def getPipeline = {
    val pipeline = Channels.pipeline()
    pipeline.addLast("smtpEncode", new SmtpEncoder)
    pipeline.addLast("smtpDecode", new SmtpDecoder)
    pipeline
  }
}

object SmtpTransporter {
  def apply(params: Stack.Params): Transporter[Request, UnspecifiedReply] =
    Netty3Transporter(SmtpPipeline, params)
}

