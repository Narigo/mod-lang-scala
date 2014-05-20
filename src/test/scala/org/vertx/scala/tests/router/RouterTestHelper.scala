package org.vertx.scala.tests.router

import org.vertx.scala.core.http.{HttpClientRequest, HttpClientResponse}
import org.vertx.testtools.VertxAssert._
import scala.concurrent.{Promise, Future}
import org.vertx.scala.core.impl.ScalaHelpers._
import org.vertx.scala.testtools.TestVerticle

/**
 * @author <a href="http://www.campudus.com/">Joern Bernhardt</a>
 */
trait RouterTestHelper extends TestVerticle {
  protected def checkBody[T](bodyCheck: String => T): HttpClientResponse => Unit = { res =>
    res.bodyHandler { body =>
      bodyCheck(body.toString("UTF-8"))
    }
  }

  protected def error404FileContents() =
    vertx.fileSystem.readFileSync("error404.html").toString("UTF-8")

  protected def testFileContents() =
    vertx.fileSystem.readFileSync("helloscala.txt").toString("UTF-8")

  protected def okCompleter[B](fn: HttpClientResponse => B): HttpClientResponse => Unit = {
    t: HttpClientResponse =>
      assertEquals(200, t.statusCode())
      completer(fn)(t)
  }

  protected def completer[A, B](fn: A => B): A => Unit = { t: A =>
    fn(t)
    testComplete()
  }

  protected def doHttp(method: String, uri: String): Future[HttpClientResponse] = {
    doHttpInternal(method, uri) { _ =>}
  }

  protected def doAuthedHttp(method: String, uri: String): Future[HttpClientResponse] = {
    doHttpInternal(method, uri) { request =>
      request.putHeader("X-XSRF-TOKEN", "secret")
    }
  }

  protected def doHttpInternal(method: String, uri: String)(fn: HttpClientRequest => Unit):
  Future[HttpClientResponse] = promisify {
    p: Promise[HttpClientResponse] =>
      val request = vertx.createHttpClient()
        .setPort(8080)
        .request(method, uri, { res => p.success(res)})
      fn(request)
      request.end()
  }

}
