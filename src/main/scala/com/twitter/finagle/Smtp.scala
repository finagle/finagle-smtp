package com.twitter.finagle

import com.twitter.util.{Await, Promise, Time, Future}
import com.twitter.finagle.client.{DefaultClient, Bridge}
import com.twitter.finagle.smtp._
import com.twitter.finagle.smtp.reply._
import com.twitter.finagle.smtp.filter.{TestHelloFilter, MailFilter, HeadersFilter, DataFilter}
import com.twitter.finagle.smtp.transport.SmtpTransporter

trait SmtpRichClient {
  /*
   * Parses Extension reply from server into SmtpExtensions object
   * that is used to define extended behaviour.
   *
   * */
  protected def availableExtensions(list: Extensions): SmtpExtensions
}

/* A client used to connect firstly and receive all supported extensions. */
private[smtp] object TestEsmtp extends Client[Request, Reply] {

  override def newClient(dest: Name, label: String) =
    TestHelloFilter andThen Smtp.defaultClient(SmtpExtensions()).newClient(dest, label)
}

/* Constructs SMTP client with extensions supported by server. */
object Smtp extends Client[Request, Reply]
  with SmtpRichClient {

  def defaultClient(ext: SmtpExtensions) = DefaultClient[Request, Reply] (
    name = "smtp",
    endpointer = {
      val bridge = Bridge[Request, UnspecifiedReply, Request, Reply](
        SmtpTransporter, new SmtpClientDispatcher(_, ext)
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
        Future.value(availableExtensions(ext))
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
    val quitOnCloseClient = new ServiceFactoryProxy[Request, Reply](Smtp.defaultClient(extensions).newClient(dest, label)){

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

    DataFilter andThen quitOnCloseClient
  }
}

/* A client that sends an email message as a request. */
object SmtpSimple extends Client[EmailMessage, Unit] {
  override def newClient(dest: Name, label: String) = {
    //send EHLO in the beginning of the session
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


