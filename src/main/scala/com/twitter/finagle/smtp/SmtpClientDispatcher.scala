package com.twitter.finagle.smtp

import com.twitter.finagle.dispatch.{GenSerialClientDispatcher, PipeliningDispatcher}
import com.twitter.finagle.transport.Transport
import com.twitter.logging.Logger
import com.twitter.util.{Future, Promise}
import org.jboss.netty.util.CharsetUtil

class SmtpClientDispatcher(trans: Transport[Request, UnspecifiedReply])
extends GenSerialClientDispatcher[Request, Reply, Request, UnspecifiedReply](trans) {
  import com.twitter.finagle.dispatch.GenSerialClientDispatcher.wrapWriteException

  /*Connection phase: should receive greeting from the server*/
  private val connPhase: Future[Unit] = {
    trans.read flatMap { greet =>
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
   * Dispatch a request, satisfying Promise `p` with the response;
   * the returned Future is satisfied when the dispatch is complete:
   * only one request is admitted at any given time.
   */
  protected def dispatch(req: Request, p: Promise[Reply]): Future[Unit] = {
    connPhase flatMap { _ =>
      p onFailure {
        case UnknownReplyCodeError(_,_) => close()
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

  private def decodeReply(rep: UnspecifiedReply, p: Promise[Reply]): Future[Unit] = {
    Reply(rep) match {
      case err: SmtpError => p.setException(err)
      case r@_ => p.setValue(r)
    }
    Future.Done
  }

}
