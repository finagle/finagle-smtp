package com.twitter.finagle.smtp

import com.twitter.finagle.dispatch.{GenSerialClientDispatcher, PipeliningDispatcher}
import com.twitter.finagle.transport.Transport
import com.twitter.logging.Logger
import com.twitter.util.{Future, Promise}
import org.jboss.netty.util.CharsetUtil

class SmtpClientDispatcher(trans: Transport[Request, UnspecifiedReply])
extends GenSerialClientDispatcher[Request, Reply, Request, UnspecifiedReply](trans) {
  import com.twitter.finagle.dispatch.GenSerialClientDispatcher.wrapWriteException
  val log = Logger(getClass)

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
      //logging
      val msg = req match {
        case ext: ExtendedRequest => ext.toChannelBuffer.toString(CharsetUtil.US_ASCII)
        case txt: TextRequest => txt.cmd
        case ent: MimeRequest => ent.mime.getMimeHeaders.mkString("","\r\n","\r\n") + ent.mime.message
      }
      log.info("client: %s", msg)

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
    if (rep.isMultiline) {
      val start = "server:\r\n" + rep.code + "-"
      val middle = rep.lines.dropRight(1).mkString("\r\n" + rep.code + "-")
      val end = "\r\n" + rep.code + " " + rep.lines.last

      log.info("%s%s%s", start, middle, end)
    }
    else log.info("server: %d %s", rep.code, rep.info)

    Reply(rep) match {
      case err: Error => p.setException(err)
      case r@_ => p.setValue(r)
    }
    Future.Done
  }

}
