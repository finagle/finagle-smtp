package com.twitter.finagle.smtp.filter

import com.twitter.finagle.smtp.extension.ExtendedMailingSession
import com.twitter.finagle.smtp.extension.chunking.ChunkingReq
import com.twitter.finagle.{Service, SimpleFilter}
import com.twitter.finagle.smtp.{Reply, Request}
import com.twitter.util.{JavaTimer, Duration, Future}
import com.twitter.util.TimeConversions._

object DefaultTimeoutsFilter extends SimpleFilter[Request, Reply]{
  implicit val timer = new JavaTimer

  def apply(request: Request, service: Service[Request, Reply]): Future[Reply] = {
    val timeout = request match {
      case Request.NewMailingSession(_) => 5.minutes
      case ExtendedMailingSession(_,_) => 5.minutes
      case Request.AddRecipient(_) => 5.minutes
      case Request.BeginData => 2.minutes
      case ChunkingReq.BeginDataChunk(_) => 3.minutes
      case ChunkingReq.BeginLastDataChunk(_) => 10.minutes
      case Request.MimeData(_) => 10.minutes
      case Request.TextData(_,_) => 10.minutes
      case _ => Duration.Top
    }

    service(request) within timeout
  }
}
