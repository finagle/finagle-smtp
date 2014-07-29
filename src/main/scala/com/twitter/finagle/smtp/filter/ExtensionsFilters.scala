package com.twitter.finagle.smtp.filter

import com.twitter.finagle.smtp._
import com.twitter.finagle.{Service, SimpleFilter}
import com.twitter.finagle.smtp.reply._
import org.jboss.netty.util.CharsetUtil
import com.twitter.util.Future
import com.twitter.finagle.smtp.SmtpExtensions

object GetExtensionFilter {
  import SmtpExtensions._
  val forSupported: PartialFunction[Extension, SimpleFilter[Request, Reply]] = {
    case Extension(EIGHTBITMIME, _) => EightBitMimeFilter
    case Extension(SIZE, size::_) => new SizeDeclarationFilter(size.toInt)
    case Extension(CHUNKING, _) => ChunkingFilter
  }

  // filters applied in case there are no extensions with such names
  val forUnsupportedExtensions = Map[String, SimpleFilter[Request, Reply]] {
    EIGHTBITMIME -> NoEightBitMimeFilter
    SIZE         -> NoSizeDeclarationFilter
    CHUNKING     -> NoChunkingFilter
    BINARYMIME   -> NoBinaryMimeFilter
    PIPELINING   -> NoPipeliningFilter
  }
}

/**
* Filter that is applied when 8BITMIME extension is not supported.
* Removes BODY=8BITMIME extension from MAIL FROM command.
* Transforms 8-bit text data into 7-bit, but discards 8-bit
* MIME-messages with RequestNotAllowed error
*/
object NoEightBitMimeFilter extends SimpleFilter[Request, Reply] {
  def apply(request: Request, service: Service[Request, Reply]) = request match {
    case Request.NewMailingSession(sender, ext) => {
      ext.get("BODY") match {
        case Some("8BITMIME") => service(Request.NewMailingSession(sender, ext - "BODY"))
        case _ => service(Request.NewMailingSession(sender, ext))
      }
    }

    case Request.TextData(text, enc) =>
      if (enc != CharsetUtil.US_ASCII) {
        val sevenBit = Request.TextData(text, CharsetUtil.US_ASCII)
        service(sevenBit)
      }
      else service(request)

    case Request.MimeData(mime) => {
      if (mime.contentTransferEncoding == "8bit") Future.value(new RequestNotAllowed)
      else service(request)
    }

    case _ => service(request)
  }
}

/**
* Filter that is applied when 8BITMIME extension is supported.
* Transforms all 8-bit text data into a MIME-message with corresponding headers.
* */
object EightBitMimeFilter extends SimpleFilter[Request, Reply] {
  def apply(request: Request, service: Service[Request, Reply]) = request match {
    case Request.TextData(text, enc) =>
      if (enc != CharsetUtil.US_ASCII) {
        val mime = Mime.plainText(text.mkString("\r\n"), enc).setContentTransferEncoding(TransferEncoding.EightBit)
        service(Request.MimeData(mime))
      }
      else service(request)

    case _ => service(request)
  }
}

/**
* Filter that is applied when SIZE extension is not supported.
* Removes SIZE extension from MAIL FROM command.
* */
object NoSizeDeclarationFilter extends SimpleFilter[Request, Reply] {
  def apply(request: Request, service: Service[Request, Reply]) = request match {
    case Request.NewMailingSession(sender, ext) => service(Request.NewMailingSession(sender, ext - "SIZE"))

    case _ => service(request)
  }
}

/**
* Filter that is applied when SIZE extension is supported.
* If declared message size is more than the value set by extension,
* completes the future with InsufficientStorageError
* and doesn't send anything.
* */
class SizeDeclarationFilter(maxSize: Int) extends SimpleFilter[Request, Reply] {
  def apply(request: Request, service: Service[Request, Reply]) = request match {
    case Request.NewMailingSession(sender, ext) =>
      if (maxSize > 0 && ext.getOrElse("SIZE", "0").toInt > maxSize)
        Future.exception(InsufficientStorageError("Message size is more than the server can accept"))
      else service(request)

    case _ => service(request)
  }
}

/**
* Filter that is applied when CHUNKING extension is not supported.
* Discards any message related to chunking with RequestNotAllowed error.
* */
object NoChunkingFilter extends SimpleFilter[Request, Reply] {
  def apply(request: Request, service: Service[Request, Reply]) = request match {
    case Request.BeginDataChunk(_) => Future.value(new RequestNotAllowed)
    case Request.BeginLastDataChunk(_) => Future.value(new RequestNotAllowed)

    case _ => service(request)
  }
}

/**
 * Filter that is applied when CHUNKING extension is supported.
 * Discards DATA requests when chunking is in progress. If the
 * transmission fails, resets the session.
 * */
object ChunkingFilter extends SimpleFilter[Request, Reply] {
  def apply(request: Request, service: Service[Request, Reply]) = request match {
    case Request.BeginDataChunk(_) => {
      service(request) onFailure {
        case _ => service(Request.Reset)
      }
    }
    case Request.BeginLastDataChunk(_) => {
      service(request) onFailure {
        case _ => service(Request.Reset)
      }
    }

    case _ => service(request)
  }
}

/**
 * Filter that is applied when BINARYMIME extension is not supported.
 * Removes BODY=BINARYMIME extension from MAIL FROM command and discards
 * binary MIME-messages with RequestNotAllowed error.
 */
object NoBinaryMimeFilter extends SimpleFilter[Request, Reply] {
  def apply(request: Request, service: Service[Request, Reply]) = request match {
    case Request.NewMailingSession(sender, ext) => {
      ext.get("BODY") match {
        case Some("BINARYMIME") => service(Request.NewMailingSession(sender, ext - "BODY"))
        case _ => service(Request.NewMailingSession(sender, ext))
      }
    }

    case Request.MimeData(mime) => {
      if (mime.contentTransferEncoding == "binary") Future.value(new RequestNotAllowed)
      else service(request)
    }

    case _ => service(request)
  }
}

/**
 * Filter that is applied when PIPELINING extension is not supported.
 * Discards grouped requests with RequestNotAllowed error.
 */
object NoPipeliningFilter extends SimpleFilter[Request, Reply] {
  def apply(request: Request, service: Service[Request, Reply]) = request match {
    case GroupedRequest(_) =>  Future.value(new RequestNotAllowed)
    case _ => service(request)
  }
}

/**
 * Filter that is applied when PIPELINING extension is supported.
 * Discards group requests with the wrong order of commands with
 * BadCommandSequence reply.
 * This filter is only added to pipelining SMTP client.
 */
object PipeliningFilter extends SimpleFilter[Request, Reply] {
  def apply(request: Request, pipeliningService: Service[Request, Reply]) = request match {
    case GroupedRequest(reqs) =>  {
      // Check command order
      val withoutLast = reqs dropRight 1
      withoutLast find {
        case _: HelloRequest => true
        case _: BeginDataRequest => true
        case Request.VerifyAddress(_) => true
        case Request.ExpandMailingList(_) => true
        case Request.Quit => true
        case Request.Noop => true
      } match {
        case Some(_) => Future.exception(BadCommandSequence("Bad command sequence in a request group"))

        // Specify and collect replies in a GroupedReply
        case None => {
          val reps = reqs map {
            pipeliningService(_) flatMap { rep =>
              Future.value(Reply(rep))
            }
          }
          Future.value(GroupedReply(reps))
        }
      }
    }

    case _ => pipeliningService(request)
  }
}