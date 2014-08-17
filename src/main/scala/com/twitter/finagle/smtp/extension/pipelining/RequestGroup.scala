package com.twitter.finagle.smtp.extension.pipelining

import com.twitter.finagle.smtp.Request
import org.jboss.netty.buffer.ChannelBuffers

case class RequestGroup(reqs: Request*) extends Request {
  // The contents (subrequests) of this request are supposed to be sent
  // separately, so the group itself cannot be sent
  def toChannelBuffer = ChannelBuffers.wrappedBuffer(Array[Byte]())
}

private[smtp] case class GroupPart(req: Request) extends Request {
  def toChannelBuffer = req.toChannelBuffer
}
