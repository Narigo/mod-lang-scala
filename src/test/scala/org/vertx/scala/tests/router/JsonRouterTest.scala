package org.vertx.scala.tests.router

import org.junit.Test
import org.vertx.testtools.VertxAssert._
import org.vertx.scala.core.impl.ScalaHelpers._
import org.vertx.scala.core.FunctionConverters._
import scala.concurrent.{Promise, Future}
import scala.util.{Try, Failure, Success}
import org.vertx.scala.core.json.Json

/**
 * @author <a href="http://www.campudus.com/">Joern Bernhardt</a>
 */
class JsonRouterTest extends RouterTestHelper {

  override def asyncBefore(): Future[Unit] = promisify { p: Promise[Unit] =>
    container.deployVerticle("scala:org.vertx.scala.tests.router.JsonRouter", handler = {
      case Success(deployId) => p.success()
      case Failure(ex) => p.failure(ex)
    }: Try[String] => Unit)
  }

  @Test def indexGet(): Unit = doHttp("GET", "/") map okCompleter(checkBody { body =>
    assertEquals(testFileContents(), body)
  })

  @Test def pathGet(): Unit = doHttp("GET", "/test.txt") map okCompleter(checkBody { body =>
    assertEquals(testFileContents(), body)
  })

  @Test def indexPost(): Unit = doHttp("POST", "/") map okCompleter(checkBody { body =>
    assertEquals("ok", Json.fromObjectString(body).getString("status"))
  })

  @Test def pathPost(): Unit = doHttp("POST", "/post-ok") map okCompleter(checkBody { body =>
    assertEquals("ok", Json.fromObjectString(body).getString("status"))
  })

  @Test def patchNotFound(): Unit = doHttp("PATCH", "/not-found") map completer { res =>
    assertEquals(404, res.statusCode())
  }

  @Test def deleteForbidden(): Unit = doHttp("DELETE", "/forbidden") map completer { res =>
    assertEquals(403, res.statusCode())
  }

  @Test def customHead(): Unit = doHttp("HEAD", "/head") map okCompleter { res =>
    assertTrue(res.headers().entryExists("x-custom-head", { h =>
      h == "hello"
    }))
  }

  @Test def allMatchesGet(): Unit = doHttp("GET", "/all-test") map okCompleter(checkBody { body =>
    assertEquals(testFileContents(), body)
  })

  @Test def allMatchesPost(): Unit = doHttp("POST", "/all-test") map okCompleter(checkBody { body =>
    assertEquals(testFileContents(), body)
  })

}
