package com.twitter.finagle.smtp

import com.twitter.finagle.transport.Transport
import com.twitter.finagle.dispatch.{PipeliningDispatcher, GenSerialClientDispatcher}
import com.twitter.util.{Future, Promise, Try}
import com.twitter.finagle.smtp.reply._
import com.twitter.logging.Logger
import org.jboss.netty.util.CharsetUtil

trait SmtpDispatcherOps[Rep, Out] { self: GenSerialClientDispatcher[Request, Rep, Request, Out] =>
  def makeUnit[T](p: Promise[T], value: => T): Future[Unit] = {
   p.updateIfEmpty(Try(value))
   Future.Done
  }

  def receiveGreeting(trans: Transport[Request, UnspecifiedReply]) = {
    trans.read flatMap { greet =>
      Reply(greet) match {
          case ServiceReady(_,_) => Future.Done
          case other => Future.exception(InvalidReply(other.toString))
      }
    } onFailure {
      case _ =>  close()
    }
  }
}

class SmtpPipeliningDispatcher(trans: Transport[Request, UnspecifiedReply])
extends PipeliningDispatcher[Request, UnspecifiedReply](trans)
with SmtpDispatcherOps[UnspecifiedReply, UnspecifiedReply]{
  /*Connection phase: should receive greeting from the server*/
  private val connPhase: Future[Unit] = receiveGreeting(trans)

  override protected def dispatch(req: Request, p: Promise[UnspecifiedReply]): Future[Unit] = {
    connPhase flatMap { _ =>
      super.dispatch(req, p)
    } onFailure {
      case _ => close()
    }
  }

}

class SmtpClientDispatcher(trans: Transport[Request, UnspecifiedReply])
extends GenSerialClientDispatcher[Request, Reply, Request, UnspecifiedReply](trans)
with SmtpDispatcherOps[Reply, UnspecifiedReply] {
  import GenSerialClientDispatcher.wrapWriteException
  val log = Logger(getClass)

  /*Connection phase: should receive greeting from the server*/
  private val connPhase: Future[Unit] = receiveGreeting(trans)

  /**
   * Dispatch a request, satisfying Promise `p` with the response;
   * the returned Future is satisfied when the dispatch is complete:
   * only one request is admitted at any given time.
   */
  protected def dispatch(req: Request, p: Promise[Reply]): Future[Unit] = {
    connPhase flatMap { _ =>
      //logging
      val msg = req match {
        case ext: ExtendedRequest => ext.toChannelBuffer.toString(CharsetUtil.US_ASCII)
        case txt: TextRequest => txt.cmd
        case ent: MimeRequest => ent.mime.getMimeHeaders.mkString("","\r\n","\r\n") + ent.mime.message
        case _ => "<Unknown request type>" //grouped request are not supposed to be dispatched here
      }
      log.info("client: %s", msg)

      trans.write(req) rescue {
        wrapWriteException
      } flatMap { unit =>
        trans.read()
      } flatMap { rp =>
        val signal = decodeReply(rp, p)
        p onFailure {
          case UnknownReplyCodeError(_,_) => close()
          case _ =>
        }
        signal
      }
    } onFailure {
      _ => close()
    }
  }

  private def decodeReply(rep: UnspecifiedReply, p: Promise[Reply]): Future[Unit] = {
    if (rep.isMultiline) {
      val start = "server:\r\n" + rep.code + "-"
      val middle = rep.lines.dropRight(1).mkString("\r\n" + rep.code + "-")
      val end = "\r\n" + rep.code + " " + rep.lines.last

      log.info("%s%s%s", start, middle, end)
    }
    else log.info("server: %d %s", rep.code, rep.info)
    makeUnit(p, Reply(rep))
  }

}
