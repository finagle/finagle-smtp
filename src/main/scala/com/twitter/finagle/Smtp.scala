package com.twitter.finagle

import com.twitter.finagle.client.{Bridge, DefaultClient}
import com.twitter.finagle.smtp._
import com.twitter.finagle.smtp.extension._
import com.twitter.finagle.smtp.filter._
import com.twitter.finagle.smtp.transport.SmtpTransporter
import com.twitter.logging.Logger
import com.twitter.util.{Future, Time}

trait SmtpRichClient { self: Client[Request, Reply] =>

  /**
   * Constructs an SMTP client which can support
   * given extensions, if they are supported by server.
   *
   * @param extensionNames Names of extensions to support; they should be made uppercase.
   */
  def withSupportFor(extensionNames: Seq[String]): Client[Request, Reply]

  /**
   * Constructs a new client that can send emails.
   */
  def newSimpleClient(dest: Name, label: String): ServiceFactory[EmailMessage, Unit]
}

object Smtp extends Client[Request, Reply] with SmtpRichClient {

  override def newClient(dest: Name, label: String) = SmtpClient().newClient(dest, label)

  def withSupportFor(extensionNames: Seq[String]) = SmtpClient(extensionNames.map(_.toUpperCase))

  def newSimpleClient(dest: Name, label: String) = SmtpSimple.newClient(dest, label)
}

/**
 * Implements an SMTP client with given extensions
 * that sends EHLO upon connection and QUIT before
 * closing connection. */
case class SmtpClient(supporting: Seq[String] = Seq.empty) extends Client[Request, Reply] {
  // TODO: switch to StackClient
  private val defaultClient = DefaultClient[Request, Reply] (
    name = "smtp",
    endpointer = {
      val bridge = Bridge[Request, UnspecifiedReply, Request, Reply](
        SmtpTransporter, new SmtpClientDispatcher(_)
      )
      (addr, stats) => bridge(addr, stats)
    })

  /**
   * Sends an EHLO request and processes its reply
   *
   * @param service The service used to send requests and receive replies
   * @return Future containing extensions supported by server
   */
  private def getExtensions(service: Service[Request, Reply]): Future[SmtpExtensions] = {
    val extService = OkToExtFilter andThen service
    extService(Request.Hello) flatMap {
      //if everything is all right, add available extensions
      case ext: Extensions => {
        val supported = for {
          line <- ext.lines.tail map { _.toUpperCase split " " }
          if supporting contains line.head
        } yield Extension(line.head, line.tail)

        // BINARYMIME can not be supported without CHUNKING
        val binary = supported find {case Extension(SmtpExtensions.BINARYMIME, _) => true}
        val chunking = supported find {case Extension(SmtpExtensions.CHUNKING, _) => true}

        val allowed = if (binary.isDefined && chunking.isEmpty) {
          supported filterNot {
            case Extension(SmtpExtensions.BINARYMIME, _) => true
            case _ => false
          }
        } else supported

        Future.value(SmtpExtensions(supported))
      }
      //else no extensions are supported
    } rescue {
      case _ => Future.value(SmtpExtensions())
    }
  }

  override def newClient(dest: Name, label: String) = {
    new ServiceFactoryProxy[Request, Reply](defaultClient.newClient(dest, label)){
      override def apply(conn: ClientConnection) = {
        self.apply(conn) flatMap { service =>
          // Send EHLO and get extensions supported by server
          getExtensions(service) flatMap { extensions =>
            // Send QUIT upon closing, if not sent manually
            val quitOnClose = new ServiceProxy[Request, Reply](service) {
              override def close(deadline: Time) = {
                if (service.isAvailable)
                  service(Request.Quit)
                service.close(deadline)
              }
            }

            Future.value {
              // Shield message body
              DataFilter andThen
              // Transform OK replies to Extensions replies
              OkToExtFilter andThen
              // Make behaviour changes according to supported extensions
              Esmtp(extensions).extend(quitOnClose)
            }
          }}}}
  }
}

/**
 * Implements an SMTP client that can send an [[com.twitter.finagle.smtp.EmailMessage]].
 * The application of this client's service returns [[com.twitter.util.Future.Done]]
 * in case of success or the first encountered error in case of a failure.
 */
object SmtpSimple extends Client[EmailMessage, Unit] {
  /**
  * Constructs an SMTP client that sends a hello request
  * in the beginning of the session to identify itself;
  * it also copies email headers into the body of the message.
  * The dot stuffing and connection closing
  * behaviour is the same as in [[com.twitter.finagle.Smtp.newClient()]].
  */
  override def newClient(dest: Name, label: String) = {
    HeadersFilter andThen MailFilter andThen Smtp.newClient(dest, label)
  }

  /**
   * Provides SMTP client that sends emails
   * and logs the session.
   *
   * @param log The logger to use
   */
  def withLogging(log: Logger) = new Client[EmailMessage, Unit] {
    def newClient(dest: Name, label: String) = {
      HeadersFilter andThen
      MailFilter andThen
      new SmtpLoggingFilter(log) andThen
      Smtp.newClient(dest, label)
    }
  }
}
