package com.twitter.finagle

import com.twitter.util.{Await, Promise, Time, Future}
import com.twitter.finagle.client.{DefaultClient, Bridge}
import com.twitter.finagle.smtp._
import com.twitter.finagle.smtp.reply._
import com.twitter.finagle.smtp.filter._
import com.twitter.finagle.smtp.transport.SmtpTransporter
import com.twitter.finagle.smtp.reply.Extensions
import com.twitter.finagle.smtp.SmtpExtensions

/* A client used to connect firstly and receive all supported extensions. */
private object TestEsmtp extends Client[Request, Reply] {
  override def newClient(dest: Name, label: String) =
    TestHelloFilter andThen Smtp.defaultClient.newClient(dest, label)
}

/* Constructs SMTP client with extensions supported by server. */
object Smtp extends Client[Request, Reply] {

  val defaultClient = DefaultClient[Request, Reply] (
    name = "smtp",
    endpointer = {
      val bridge = Bridge[Request, UnspecifiedReply, Request, Reply](
        SmtpTransporter, new SmtpClientDispatcher(_)
      )
      (addr, stats) => bridge(addr, stats)
    })

  override def newClient(dest: Name, label: String) = {
    val client = Promise[ServiceFactory[Request, Reply]]
    val extService = TestEsmtp.newService(dest, label)
    extService(Request.Hello) flatMap {
      //if everything is all right, add available extensions
      case ext: Extensions => {
        extService(Request.Quit)
        val esmtp = SmtpExtensions( ext.lines.tail map { _.toUpperCase } )
        Future.value(esmtp)
      }
      //else construct client without extensions
      case _ => Future.value(SmtpExtensions())
    } onSuccess { extensions =>
      client.setValue(SmtpClient(extensions).newClient(dest, label))
    }

    Await.result(client)
  }
}


/* Implements an SMTP client with given extensions that sends QUIT before closing connection. */
case class SmtpClient(extensions: SmtpExtensions) extends Client[Request, Reply] {
  override def newClient(dest: Name, label: String) = {

    val quitOnCloseClient = new ServiceFactoryProxy[Request, Reply](Smtp.defaultClient.newClient(dest, label)){

      override def apply(conn: ClientConnection) = {

        self.apply(conn) flatMap { service =>

          val quitOnClose = new ServiceProxy[Request, Reply](service) {

            override def close(deadline: Time) = {

              if (service.isAvailable)
                service(Request.Quit)
              service.close(deadline)
            }
          }
          Future.value(quitOnClose)
        }
      }
    }

    DataFilter andThen new ExtensionsFilter(extensions) andThen quitOnCloseClient
  }
}

/* A client that sends an email message as a request. */
object SmtpSimple extends Client[EmailMessage, Unit] {
  /**
  * Constructs an SMTP client that sends a hello request
  * in the beginning of the session to identify itself;
  * it also copies email headers into the body of the message.
  * The dot stuffing and connection closing
  * behaviour is the same as in [[com.twitter.finagle.Smtp.newClient()]].
  */
  override def newClient(dest: Name, label: String): ServiceFactory[EmailMessage, Unit] = {
    val startHelloClient = new ServiceFactoryProxy[Request, Reply](Smtp.newClient(dest, label)) {
      override def apply(conn: ClientConnection) = {
        self.apply(conn) flatMap { service =>
          service(Request.Hello)
          Future.value(service)
        }
      }
    }
    HeadersFilter andThen MailFilter andThen startHelloClient
  }
}


