package io.github.finagle.smtp.extension.auth

import com.twitter.util.Base64StringEncoder
import org.jboss.netty.util.CharsetUtil

/**
 * Represents SASL authentication mechanism.
 *
 * @param name Name of the mechanism
 * @param reply The function which transforms server challenges to client responses
 * @param initialResponse The initial response, if the mechanism can use it.
 */
case class AuthMechanism (
  name: String,
  reply: ServerChallenge => ChallengeResponse,
  initialResponse: Option[String] = None
  )

/**
 * Contains predefined authentication mechanisms.
 */
object AuthMechanism {
  /**
   * Constructs PLAIN authentication mechanism using login as
   * authentication identity and provided password. Here initial
   * response is not used.
   */
  def plain(login: String, password: String): AuthMechanism = AuthMechanism(
    "PLAIN",
    ch => {
      val authString = login + '\0' + password
      ChallengeResponse(Base64StringEncoder.encode(authString.getBytes(CharsetUtil.UTF_8)))
    },
    None
  )
}