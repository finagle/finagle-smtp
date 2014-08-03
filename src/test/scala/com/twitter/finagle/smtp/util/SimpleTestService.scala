package com.twitter.finagle.smtp.util

import com.twitter.finagle.Service
import com.twitter.finagle.smtp.{Reply, Request}
import com.twitter.util.Future

object SimpleTestService extends Service[Request, Reply] {
  def apply(req: Request): Future[Reply] = Future { TestReply(req)}
}
