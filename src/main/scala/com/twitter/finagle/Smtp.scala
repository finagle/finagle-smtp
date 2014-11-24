package com.twitter.finagle

import com.twitter.finagle.Stack.Role
import com.twitter.finagle.client.{StackClient, StdStackClient}
import com.twitter.finagle.smtp._
import com.twitter.finagle.smtp.extension._
import com.twitter.finagle.smtp.filter._
import com.twitter.finagle.smtp.transport.SmtpTransporter
import com.twitter.finagle.transport.Transport
import com.twitter.logging.Logger

object SmtpLogging {
  case class LoggerParam(log: Option[Logger])
  implicit object LoggerParam extends Stack.Param[LoggerParam] {
    val default = LoggerParam(None)
  }

  object Stackable extends Stack.Module1[LoggerParam, ServiceFactory[Request, Reply]] {
    val role = Role("SmtpLoggingFilter")
    val description = "Logs SMTP session with a given Logger"

    def make(p1: LoggerParam, next: ServiceFactory[Request, Reply]) = p1.log match {
      case Some(log) => SmtpLoggingFilter(log) andThen next
      case None => next
    }
  }
}

/**
 * Creates different types of clients
 */
object Smtp extends com.twitter.finagle.Client[Request, Reply] {

  /**
   * Implements an SMTP client with given extensions
   * that greets server upon connection.
   */
  case class Client(
    stack: Stack[ServiceFactory[Request, Reply]] =
      StackClient.newStack.+:(Esmtp.Stackable).+:(SmtpLogging.Stackable),
    params: Stack.Params = StackClient.defaultParams
    ) extends StdStackClient[Request, Reply, Client] {
    protected def copy1(
      stack: Stack[ServiceFactory[Request, Reply]] = this.stack,
      params: Stack.Params = this.params
    ): Client = copy(stack, params)

    protected type In = Request
    protected type Out = UnspecifiedReply
    protected def newTransporter() = SmtpTransporter(params)
    protected def newDispatcher(
      transport: Transport[Request, UnspecifiedReply]): Service[Request, Reply] =
      new ClientDispatcher(transport)

    /**
     * Constructs an SMTP client which can support
     * given extensions, if they are supported by server.
     *
     * @param extensions Extensions to support
     */
    def withSupportFor(extensions: Extensions.ExtensionList): Client = configured(extensions)

    /**
     * Constructs SMTP client which can log the session.
     *
     * @param log The logger to use
     */
    def withLogging(log: Logger): Client = configured(SmtpLogging.LoggerParam(Some(log)))

    /**
     * Constructs an SMTP client that can send
     * [[com.twitter.finagle.smtp.EmailMessage EmailMessages]].
     * The application of this client's service returns [[com.twitter.util.Future.Done]]
     * in case of success or the first encountered error in case of a failure.
     */
    def simple = {
      val self = this
      new com.twitter.finagle.Client[EmailMessage, Unit] {
        def newClient(dest: Name, label: String) =
          HeadersFilter andThen MailFilter andThen
            DefaultTimeoutsFilter andThen self.newClient(dest, label)
    }
    }
  }

  val client = Client()

  def newClient(dest: Name, label: String) = client.newClient(dest, label)
}
