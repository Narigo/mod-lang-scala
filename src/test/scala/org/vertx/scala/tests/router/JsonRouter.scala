package org.vertx.scala.tests.router

import org.vertx.scala.router.{RouterException, Router}
import org.vertx.scala.core.http.{HttpServer, HttpServerRequest}
import org.vertx.scala.core.FunctionConverters._
import org.vertx.scala.router.routing._
import org.vertx.scala.platform.Verticle
import scala.concurrent.Promise
import scala.util.{Try, Failure, Success}
import org.vertx.scala.core.json.Json

/**
 * @author <a href="http://www.campudus.com/">Joern Bernhardt</a>
 */
class JsonRouter extends Verticle with Router {

  override def start(p: Promise[Unit]) = {
    vertx.createHttpServer().requestHandler(this).listen(8080, {
      case Success(s) => p.success()
      case Failure(x) => p.failure(x)
    }: Try[HttpServer] => Unit)
  }

  override def routes(req: HttpServerRequest) = {
    case Get("/") => SendFile("helloscala.txt")
    case Get("/test.txt") => SendFile("helloscala.txt")
    case Post("/") => Ok(Json.obj("status" -> "ok"))
    case Post("/post-ok") => Ok(Json.obj("status" -> "ok"))
    case Head("/") => Header("x-custom-head", "hello", NoBody)
    case Delete("/forbidden") => Error(RouterException(message = "not authorized", statusCode = 403))
    case All("/all-test") => Ok(Json.obj("status" -> "ok"))
  }
}
