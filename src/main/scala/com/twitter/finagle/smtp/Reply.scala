package com.twitter.finagle.smtp

import com.twitter.finagle.smtp.extension.auth.{AuthRejected, AuthSuccessful, ServerChallenge}
import com.twitter.util.Future

trait UnspecifiedReply {
  val code: Int
  val info: String

  val isMultiline: Boolean = false
  val lines: Seq[String] = Seq(info)
}

case class NonTerminalLine(code: Int, info: String) extends UnspecifiedReply

object ReplyCode {

  val SYSTEM_STATUS               = 211
  val HELP                        = 214
  val SERVICE_READY               = 220
  val CLOSING_TRANSMISSION        = 221
  val OK_REPLY                    = 250
  val TEMP_USER_NOT_LOCAL         = 251
  val TEMP_USER_NOT_VERIFIED      = 252
  val START_INPUT                 = 354
  val SERVICE_NOT_AVAILABLE       = 421
  val TEMP_MAILBOX_UNAVAILABLE    = 450
  val PROCESSING_ERROR            = 451
  val TEMP_INSUFFICIENT_STORAGE   = 452
  val PARAMS_ACCOMODATION_ERROR   = 455
  val SYNTAX_ERROR                = 500
  val ARGUMENT_SYNTAX_ERROR       = 501
  val COMMAND_NOT_IMPLEMENTED     = 502
  val BAD_COMMAND_SEQUENCE        = 503
  val PARAMETER_NOT_IMPLEMENTED   = 504
  val MAILBOX_UNAVAILABLE_ERROR   = 550
  val USER_NOT_LOCAL_ERROR        = 551
  val INSUFFICIENT_STORAGE_ERROR  = 552
  val INVALID_MAILBOX_NAME        = 553
  val TRANSACTION_FAILED          = 554
  val ADDRESS_NOT_RECOGNIZED      = 555

  val INVALID_REPLY_CODE          = -1
  val GROUPED_REPLY               = 000
}

import com.twitter.finagle.smtp.ReplyCode._
import com.twitter.finagle.smtp.extension.auth.AuthReplyCode._

trait Reply extends UnspecifiedReply

case class GroupedReply(val reps: Seq[Future[Reply]]) extends Reply {
  val code = GROUPED_REPLY
  val info = ""
}

object Reply {
  def apply(rep: UnspecifiedReply) = rep match {
    case specified: Reply => specified
    case _: UnspecifiedReply => {
      rep.code match {
        case SYSTEM_STATUS              => new SystemStatus(rep.info) {
          override val isMultiline = rep.isMultiline
          override val lines = rep.lines
        }
        case HELP                       => new Help(rep.info)  {
          override val isMultiline = rep.isMultiline
          override val lines = rep.lines
        }
        case SERVICE_READY              => val (domain, info) = rep.info span {_ != ' '}
          new ServiceReady(domain, info)  {
            override val isMultiline = rep.isMultiline
            override val lines = rep.lines
          }
        case CLOSING_TRANSMISSION       => new ClosingTransmission(rep.info)  {
          override val isMultiline = rep.isMultiline
          override val lines = rep.lines
        }
        case OK_REPLY                   => new OK(rep.info)  {
          override val isMultiline = rep.isMultiline
          override val lines = rep.lines
        }
        case TEMP_USER_NOT_LOCAL        => new TempUserNotLocal(rep.info)  {
          override val isMultiline = rep.isMultiline
          override val lines = rep.lines
        }
        case TEMP_USER_NOT_VERIFIED     => new TempUserNotVerified(rep.info)  {
          override val isMultiline = rep.isMultiline
          override val lines = rep.lines
        }
        case START_INPUT                => new StartInput(rep.info)  {
          override val isMultiline = rep.isMultiline
          override val lines = rep.lines
        }
        case SERVICE_NOT_AVAILABLE      => new ServiceNotAvailable(rep.info)  {
          override val isMultiline = rep.isMultiline
          override val lines = rep.lines
        }
        case TEMP_MAILBOX_UNAVAILABLE   => new TempMailboxUnavailable(rep.info)  {
          override val isMultiline = rep.isMultiline
          override val lines = rep.lines
        }
        case PROCESSING_ERROR           => new ProcessingError(rep.info)  {
          override val isMultiline = rep.isMultiline
          override val lines = rep.lines
        }
        case TEMP_INSUFFICIENT_STORAGE  => new TempInsufficientStorage(rep.info)  {
          override val isMultiline = rep.isMultiline
          override val lines = rep.lines
        }
        case PARAMS_ACCOMODATION_ERROR  => new ParamsAccommodationError(rep.info)  {
          override val isMultiline = rep.isMultiline
          override val lines = rep.lines
        }
        case SYNTAX_ERROR               => new SyntaxError(rep.info)  {
          override val isMultiline = rep.isMultiline
          override val lines = rep.lines
        }
        case ARGUMENT_SYNTAX_ERROR      => new ArgumentSyntaxError(rep.info)  {
          override val isMultiline = rep.isMultiline
          override val lines = rep.lines
        }
        case COMMAND_NOT_IMPLEMENTED    => new CommandNotImplemented(rep.info)  {
          override val isMultiline = rep.isMultiline
          override val lines = rep.lines
        }
        case BAD_COMMAND_SEQUENCE       => new BadCommandSequence(rep.info)  {
          override val isMultiline = rep.isMultiline
          override val lines = rep.lines
        }
        case PARAMETER_NOT_IMPLEMENTED  => new ParameterNotImplemented(rep.info)  {
          override val isMultiline = rep.isMultiline
          override val lines = rep.lines
        }
        case MAILBOX_UNAVAILABLE_ERROR  => new MailboxUnavailableError(rep.info)  {
          override val isMultiline = rep.isMultiline
          override val lines = rep.lines
        }
        case USER_NOT_LOCAL_ERROR       => new UserNotLocalError(rep.info)  {
          override val isMultiline = rep.isMultiline
          override val lines = rep.lines
        }
        case INSUFFICIENT_STORAGE_ERROR => new InsufficientStorageError(rep.info)  {
          override val isMultiline = rep.isMultiline
          override val lines = rep.lines
        }
        case INVALID_MAILBOX_NAME       => new InvalidMailboxName(rep.info)  {
          override val isMultiline = rep.isMultiline
          override val lines = rep.lines
        }
        case TRANSACTION_FAILED         => new TransactionFailed(rep.info)  {
          override val isMultiline = rep.isMultiline
          override val lines = rep.lines
        }
        case ADDRESS_NOT_RECOGNIZED     => new AddressNotRecognized(rep.info)  {
          override val isMultiline = rep.isMultiline
          override val lines = rep.lines
        }

        // Authentication
        case SERVER_CHALLENGE           => new ServerChallenge(rep.info)  {
          override val isMultiline = rep.isMultiline
          override val lines = rep.lines
        }
        case AUTH_REJECTED              => new AuthRejected(rep.info)  {
          override val isMultiline = rep.isMultiline
          override val lines = rep.lines
        }
        case AUTH_SUCCESSFUL            => new AuthSuccessful(rep.info)  {
          override val isMultiline = rep.isMultiline
          override val lines = rep.lines
        }

        // Unknown replies
        case _                          => new UnknownReplyCodeError(rep.code, rep.info)  {
          override val isMultiline = rep.isMultiline
          override val lines = rep.lines
        }
      }
    }
  }
}

