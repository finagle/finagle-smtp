package com.twitter.finagle.smtp

import com.twitter.finagle.dispatch.{GenSerialClientDispatcher, PipeliningDispatcher}
import com.twitter.finagle.smtp.ReplyCode.Reply
import com.twitter.finagle.smtp.extension.pipelining.GroupPart
import com.twitter.finagle.transport.Transport
import com.twitter.util.TimeConversions._
import com.twitter.util.{Future, JavaTimer, Promise, Time}

/**
 * A ClientDispatcher that implements SMTP client/server protocol.
 */
class ClientDispatcher(trans: Transport[Request, UnspecifiedReply])
  extends GenSerialClientDispatcher[Request, Reply, Request, UnspecifiedReply](trans) {
  import com.twitter.finagle.dispatch.GenSerialClientDispatcher.wrapWriteException

  implicit val timer = new JavaTimer

  /**
   * Performs the connection phase. This is done once
   * before any client-server exchange. Upon the connection
   * the server should send greeting. If the greeting is
   * malformed, the service is closed when trying to dispatch
   * a request.
   */
  private val connPhase: Future[Unit] = {
    trans.read.within(5.minutes) flatMap { greet =>
      Reply(greet) match {
        case ServiceReady(_,_) => Future.Done
        case other => Future.exception(InvalidReply(other.toString))
      }
    }
  }

  /**
   * The dispatcher used for pipelining commands when ''PIPELINING''
   * extension is supported.
   */
  private lazy val pipeliningDispatcher = new PipeliningDispatcher[Request, UnspecifiedReply](trans)

  /**
   * Reads a reply or a sequence of replies (parts of a multiline reply).
   * Every line of a multiline reply is a
   * [[com.twitter.finagle.smtp.NonTerminalLine]]. Once
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
   * If their codes are not matching, an [[com.twitter.finagle.smtp.InvalidReply]]
   * is returned.
   */
  private def multilineReply(replies: Seq[UnspecifiedReply]): UnspecifiedReply = {
    val lns = replies.map(_.info).reverse
    val valid =
      replies.map(_.code).distinct.length == 1 &&
      replies.collectFirst { case InvalidReply(_) => true } .isEmpty

    if (valid)
      new UnspecifiedReply{
        val code = replies.head.code
        val info = lns.mkString("\r\n")
      }
    else
      new InvalidReply(lns.mkString("\r\n")) {
        override val code = replies.last.code
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

  override def close(deadline: Time) = connPhase flatMap { _ =>
    apply(Request.Quit).unit within 2.minutes
  } ensure super.close(deadline)

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
