package io.github.finagle.smtp

import com.twitter.util.Await
import io.github.finagle.Smtp
import io.github.finagle.smtp.extension.auth.AuthRequired
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
    val service = Smtp.client.simple.newService("smtp.gmail.com:25")
    service(DefaultEmail().from_("example@example.org")) onSuccess { _ =>
      fail("expected to fail")
    } handle {
      case AuthRequired(_) =>
      case _ => fail("expected AuthRequired")
    }

  }
}
