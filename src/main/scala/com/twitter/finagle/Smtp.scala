package com.twitter.finagle

import com.twitter.logging.Logger
import com.twitter.util.{Await, Time, Future}
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
    TestHelloFilter andThen OkToExtFilter andThen Clients.defaultClient.newClient(dest, label)
}

private object Clients {
  val defaultClient = DefaultClient[Request, Reply] (
    name = "smtp",
    endpointer = {
      val bridge = Bridge[Request, UnspecifiedReply, Request, Reply](
        SmtpTransporter, new SmtpClientDispatcher(_)
      )
      (addr, stats) => bridge(addr, stats)
    })

  val pipeliningClient = DefaultClient[Request, UnspecifiedReply] (
    name = "smtp",
    endpointer = {
      val bridge = Bridge[Request, UnspecifiedReply, Request, UnspecifiedReply](
        SmtpTransporter, new SmtpPipeliningDispatcher(_)
      )
      (addr, stats) => bridge(addr, stats)
    })
}

trait SmtpRichClient { self: Client[Request, Reply] =>
  /**
   * Constructs a new SMTP client that can pipeline requests.
   * If pipelining is not allowed (no PIPELINING extension),
   * an ordinary SMTP client is constructed.
   */
  def newPipeliningClient(dest: Name, label: String): ServiceFactory[Request, Reply]

  /**
   * Constructs a new client that can send emails.
   */
  def newSimpleClient(dest: Name, label: String): ServiceFactory[EmailMessage, Unit]
}

object Smtp extends Client[Request, Reply] with SmtpRichClient {
  private def getExtensions(dest: Name, label: String): SmtpExtensions = {
    val extService = TestEsmtp.newService(dest, label)
    val extensions = extService(Request.Hello) flatMap {
      //if everything is all right, add available extensions
      case ext: Extensions => {
        extService(Request.Quit)
        val supported = for {
          line <- ext.lines.tail map { _.toUpperCase split " " }
        } yield Extension(line.head, line.tail)
        Future.value(SmtpExtensions(supported))
      }
      //else no extensions are supported
    } rescue {
      case _ => Future.value(SmtpExtensions())
    }

    Await.result(extensions)
  }

  override def newClient(dest: Name, label: String) = {
    val esmtp = getExtensions(dest, label)
    SmtpClient(esmtp).newClient(dest, label)
  }

  def newPipeliningClient(dest: Name, label: String) = {
    val esmtp = getExtensions(dest, label)
    SmtpPipelining(esmtp).newClient(dest, label)
  }

  def newSimpleClient(dest: Name, label: String) = SmtpSimple.newClient(dest, label)
}

//adds extensions to SMTP clients
case class Esmtp(extensions: SmtpExtensions) {
  // filters for supported SMTP extensions
  lazy val supportedExtFilters = for {
    extension <- extensions.supported
    if GetExtensionFilter.forSupported.isDefinedAt(extension)
  } yield GetExtensionFilter forSupported extension

  // filters applied when some SMTP extensions are not supported
  lazy val unsupportedExtFilters = for {
    (extension, filter) <- GetExtensionFilter.forUnsupportedExtensions
    if !(extensions.supported.map(_.keyword) contains extension)
  } yield filter

  lazy val extFilters = (supportedExtFilters ++ unsupportedExtFilters)
                         .reduceRight[Filter[Request, Reply, Request, Reply]] { _ andThen _}

  def extend(client: ServiceFactory[Request, Reply]): ServiceFactory[Request, Reply] = {
    DataFilter andThen
    OkToExtFilter andThen
    extFilters andThen
    client
  }
}

/* Implements an SMTP client with given extensions that sends QUIT before closing connection. */
case class SmtpClient(extensions: SmtpExtensions) extends Client[Request, Reply] {

  override def newClient(dest: Name, label: String) = {
    val quitOnCloseClient = new ServiceFactoryProxy[Request, Reply](Clients.defaultClient.newClient(dest, label)){
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

    Esmtp(extensions) extend quitOnCloseClient
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

/**
 * An SMTP client that can pipeline requests,
 * if there is pipelining support.
 */
private case class SmtpPipelining(extensions: SmtpExtensions) extends Client[Request, Reply] {
  override def newClient(dest: Name, label: String) = {
    val client = extensions.supported find {
      case Extension(SmtpExtensions.PIPELINING, _) => true
      case _ => false
    } match {
      case Some(_) => PipeliningFilter andThen SpecifyingFilter andThen Clients.pipeliningClient.newClient(dest, label)
      case None => Smtp.newClient(dest, label)
    }

    Esmtp(extensions) extend client
  }
}
