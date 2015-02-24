package io.github.finagle.smtp.util

import io.github.finagle.smtp.{Reply, Request}

//a reply that holds request as it is sent to the service
case class TestReply(req: Request) extends Reply {
  val code = 0
  val info = "test ok"
}
