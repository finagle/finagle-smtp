package com.twitter.finagle.smtp

import com.twitter.finagle.dispatch.{GenSerialClientDispatcher, PipeliningDispatcher}
import com.twitter.finagle.smtp.extension.pipelining.GroupPart
import com.twitter.finagle.transport.Transport
import com.twitter.util.TimeConversions._
import com.twitter.util.{Promise, Future}

/**
 * A ClientDispatcher that implements SMTP client/server protocol.
 */
class SmtpClientDispatcher(trans: Transport[Request, UnspecifiedReply])
extends GenSerialClientDispatcher[Request, Reply, Request, UnspecifiedReply](trans){
  import GenSerialClientDispatcher.wrapWriteException
  import ReplyCode._

  /**
   * Performs the connection phase. This is done once
   * before any client-server exchange. Upon the connection
   * the server should send greeting. If the greeting is
   * not correct, the service is closed.
   */
  private val connPhase: Future[Unit] = {
    trans.read.within(5.minutes) flatMap { greet =>
      Reply(greet) match {
        case ServiceReady(_,_) => Future.Done
        case other => Future.exception(InvalidReply(other.toString))
      }
    } onFailure {
      case _ =>  close()
    }
  }

  private lazy val pipeliningDispatcher = new PipeliningDispatcher[Request, UnspecifiedReply](trans)

  /**
   * Dispatch and log a request, satisfying Promise `p` with the response;
   * the returned Future is satisfied when the dispatch is complete:
   * only one request is admitted at any given time.
   */
  protected def dispatch(req: Request, p: Promise[Reply]): Future[Unit] = {
    connPhase flatMap { _ =>
      p onFailure {
        case UnknownReplyCodeError(_,_) => this.apply(Request.Reset).unit
        case _ =>
      }

      req match {
        case GroupPart(rq) => pipeliningDispatcher(rq) flatMap { rp =>
          decodeReply(rp, p)
        }
        case _ =>
          trans.write(req) rescue {
            wrapWriteException
          } flatMap { unit =>
            trans.read()
          } flatMap { rp =>
            decodeReply(rp, p)
          }
      }

    } onFailure {
      _ => close()
    }
  }

  /**
   * Satisfies given promise with the specified version of a given reply
   * and logs it.
   *
   * @param rep The reply to specify
   * @param p   The satisfied promise
   */
  private def decodeReply(rep: UnspecifiedReply, p: Promise[Reply]): Future[Unit] = {
    Reply(rep) match {
      case err: SmtpError => p.setException(err)
      case r@_ => p.setValue(r)
    }
    Future.Done
  }

}
