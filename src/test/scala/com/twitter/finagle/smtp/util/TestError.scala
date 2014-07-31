package com.twitter.finagle.smtp.util

/**
 * Created by lenovo on 30.07.2014.
 */
class TestError extends Error {
  val code = -1
  val info = "test error"
}
