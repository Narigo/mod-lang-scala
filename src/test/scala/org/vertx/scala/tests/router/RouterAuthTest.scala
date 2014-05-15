package org.vertx.scala.tests.router

import scala.concurrent.{Promise, Future}
import org.vertx.scala.core.impl.ScalaHelpers._
import org.vertx.scala.core.FunctionConverters._
import scala.util.Try
import org.junit.Test
import org.vertx.testtools.VertxAssert._
import scala.util.Failure
import scala.util.Success

/**
 * @author <a href="http://www.campudus.com/">Joern Bernhardt</a>
 */
class RouterAuthTest extends RouterTestHelper {
  override def asyncBefore(): Future[Unit] = promisify { p: Promise[Unit] =>
    container.deployVerticle("scala:org.vertx.scala.tests.router.AuthRouter", handler = {
      case Success(deployId) => p.success()
      case Failure(ex) => p.failure(ex)
    }: Try[String] => Unit)
  }

  @Test def unauthorizedGet(): Unit = doHttp("GET", "/member") map completer { res =>
    assertEquals("Should get a 403-FORBIDDEN status code", 403, res.statusCode())
  }

  @Test def authorizedGet(): Unit = doAuthedHttp("GET", "/member") map okCompleter(checkBody {
    body =>
      assertEquals("Should receive the real test file contents", testFileContents(), body)
  })

  @Test def wrongAuthTokenGet(): Unit = doHttpInternal("GET", "/member") { req =>
    req.putHeader("X-XSRF-TOKEN", "wrong")
  } map completer { res =>
    assertEquals("Should get a 403-FORBIDDEN status code", 403, res.statusCode())
  }
}
