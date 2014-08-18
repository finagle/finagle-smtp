package com.twitter.finagle.smtp.extension.pipelining

import com.twitter.finagle.smtp.Request
import org.jboss.netty.buffer.{ChannelBuffer, ChannelBuffers}

/**
 * The group of requests sent when using PIPELINING extension.
 */
case class RequestGroup(reqs: Request*) extends Request {
  // The contents (subrequests) of this request are supposed to be sent
  // separately, so the group itself cannot be sent
  def toChannelBuffer(): ChannelBuffer = ChannelBuffers.wrappedBuffer(Array[Byte]())
}

/**
 * A part of a request group. Used internally when processing request groups.
 */
private[smtp] case class GroupPart(req: Request) extends Request {
  def toChannelBuffer() = req.toChannelBuffer
}
