package com.twitter.finagle.smtp

import com.twitter.finagle.dispatch.{GenSerialClientDispatcher, PipeliningDispatcher}
import com.twitter.finagle.smtp.extension.pipelining.GroupPart
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
   * Reads a reply or a sequence of replies (parts of a multiline reply).
   * Every line of a multiline reply is a
   * [[com.twitter.finagle.smtp.reply.NonTerminalLine]]. Once
   * anything else is received, the reply is counted as complete.
   */
  private def readLines: Future[Seq[UnspecifiedReply]] = {
    trans.read() flatMap  {
      case line: NonTerminalLine => readLines map {lines => lines :+ line}
      case other => Future.value(Seq(other))
    }
  }

  /**
   * Constructs a multiline reply from given sequence of replies.
   * If their codes are not matching, an [[com.twitter.finagle.smtp.reply.InvalidReply]]
   * is returned.
   */
  private def multilineReply(replies: Seq[UnspecifiedReply]): UnspecifiedReply = {
    val lns = replies.map(_.info).reverse
    val valid =
      replies.map(_.code).distinct.length == 1 &&
      replies.collectFirst { case InvalidReply(_) => true } .isEmpty

    // Since we changed code in invalidReply, it will be cast correctly
    if (valid)
      new UnspecifiedReply{
        val code = replies.head.code
        val info = lns.head
        override val isMultiline = true
        override val lines = lns
      }
    else
      new InvalidReply(lns.head) {
        override val code = replies.last.code
        override val isMultiline = true
        override val lines = lns
      }
  }

  /**
   * Dispatch and log a request, satisfying Promise `p` with the response;
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
