package com.twitter.finagle.smtp

import com.twitter.finagle.Smtp
import com.twitter.finagle.smtp.extension.auth.AuthRequired
import com.twitter.util.Await
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class IntegrationTest extends FunSuite  {

  test("noop") {
    val service = Smtp.newService("smtp.gmail.com:25")
    val rep = Await result service(Request.Noop)

    assert(rep.isInstanceOf[OK])
  }

  test("unauthenticated email results in error") {
    val service = Smtp.newSimpleService("smtp.gmail.com:25")
    service(DefaultEmail().from_("example@example.org")) onSuccess { _ =>
      fail("expected to fail")
    } handle {
      case AuthRequired(_) =>
      case _ => fail("expected AuthRequired")
    }

  }
}
