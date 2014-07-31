package com.twitter.finagle.smtp.extension

import com.twitter.finagle.smtp.Request
import com.twitter.finagle.smtp.filter.TestHelloFilter
import com.twitter.finagle.smtp.reply.Reply
import com.twitter.finagle.{Client, Name}

/**
 * Created by lenovo on 31.07.2014.
 */
/* A client used to connect firstly and receive all supported extensions. */
private object TestEsmtp extends Client[Request, Reply] {
  override def newClient(dest: Name, label: String) =
    TestHelloFilter andThen OkToExtFilter andThen Clients.defaultClient.newClient(dest, label)
}
