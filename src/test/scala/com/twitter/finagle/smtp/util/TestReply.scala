package com.twitter.finagle.smtp.util

import com.twitter.finagle.smtp.Request
import com.twitter.finagle.smtp.reply.Reply

/**
 * Created by lenovo on 30.07.2014.
 */
//a reply that holds request as it is sent to the service
case class TestReply(req: Request) extends Reply {
  val code = 0
  val info = "test ok"
}
