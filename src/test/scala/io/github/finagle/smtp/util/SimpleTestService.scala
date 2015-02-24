package io.github.finagle.smtp.util

import com.twitter.finagle.Service
import com.twitter.util.Future
import io.github.finagle.smtp.{Reply, Request}

object SimpleTestService extends Service[Request, Reply] {
  def apply(req: Request): Future[Reply] = Future { TestReply(req)}
}
