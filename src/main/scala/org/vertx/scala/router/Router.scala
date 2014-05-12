package org.vertx.scala.router

import org.vertx.scala.core.http.{HttpServerResponse, HttpServerRequest}
import org.vertx.scala.router.routing._
import org.vertx.scala.core._
import scala.concurrent.Future
import org.vertx.scala.core.file.FileProps
import java.io.FileNotFoundException
import org.vertx.scala.core.impl.ScalaHelpers._
import org.vertx.scala.router.routing.AsyncReply
import org.vertx.scala.router.routing.Connect
import org.vertx.scala.router.routing.SendFile
import org.vertx.scala.router.routing.Get
import org.vertx.scala.router.routing.Delete
import org.vertx.scala.router.routing.All
import org.vertx.scala.router.routing.Options
import org.vertx.scala.router.routing.Put
import org.vertx.scala.router.routing.Trace
import org.vertx.scala.router.routing.Ok
import org.vertx.scala.router.routing.Error
import org.vertx.scala.router.routing.Patch
import org.vertx.scala.router.routing.SetCookie
import org.vertx.scala.router.routing.Post
import org.vertx.scala.router.routing.Head
import java.net.URLEncoder

/**
 * The Router trait can be extended to give access to an easy way to write nice routes for your HTTP server.
 *
 * @author <a href="http://www.campudus.com/">Joern Bernhardt</a>
 */
trait Router extends (HttpServerRequest => Unit) {
  this: VertxAccess =>
  private implicit val executionContext = VertxExecutionContext.fromVertx(vertx, logger)

  private val noRouteMatch: RouteMatch => Reply =
    _ => Error(RouterException(message = "No route matched.", id = "NO_ROUTE", statusCode = 404))

  private def matcherFor(routeMatch: RouteMatch, req: HttpServerRequest): Reply = {
    val pf: PartialFunction[RouteMatch, Reply] = routes(req)
    val tryAllThenNoRouteMatch: Function[RouteMatch, Reply] = _ => pf.applyOrElse(All(req.path()), noRouteMatch)
    pf.applyOrElse(routeMatch, tryAllThenNoRouteMatch)
  }

  private def fileExists(file: String): Future[String] = asyncResultPromisify { tryFn: ResultHandler[Boolean] =>
    vertx.fileSystem.exists(file, tryFn)
  } map {
    case true => file
    case false => throw new FileNotFoundException(file)
  }

  private def addIndexToDirName(path: String): String =
    if (path.endsWith("/")) path + "index.html"
    else path + "/index.html"

  private def directoryToIndexFile(path: String): Future[String] = asyncResultPromisify { tryFn: ResultHandler[FileProps] =>
    vertx.fileSystem.lprops(path, tryFn)
  } flatMap { fp =>
    if (fp.isDirectory) fileExists(addIndexToDirName(path))
    else Future.successful(path)
  }

  private def urlEncode(str: String) = URLEncoder.encode(str, "UTF-8")

  private def endResponse(resp: HttpServerResponse, reply: SyncReply): Unit = {
    logger.info(s"got a SyncReply to send back as response in endResponse: $reply")

    reply match {
      case NoBody =>
        resp.end()
      case Ok(js) =>
        resp.setStatusCode(200)
        resp.end(js.encode())
      case SendFile(path) =>
        fileExists(path) flatMap directoryToIndexFile map { file =>
          logger.info(s"Serving file $file after receiving request for: $path")
          resp.sendFile(file, notFoundFile)
        }
      case Error(RouterException(message, cause, id, statusCode)) =>
        logger.warn(s"Got an error $statusCode: $message", cause)
        resp.setStatusCode(statusCode)
        resp.end(message)
    }
  }

  // TODO using collection.mutable.Set() usage is not perfect...
  private def sendReply(req: HttpServerRequest, reply: Reply): Unit = {
    logger.info(s"got a Reply to send back as response: $reply")

    reply match {
      case AsyncReply(future) =>
        future.map(x => sendReply(req, x)).recover {
          case ex: RouterException => endResponse(req.response(), errorReplyFromException(ex))
          case ex: Throwable => endResponse(req.response(), Error(routerException(ex)))
        }
      case SetCookie(key, value, nextReply) =>
        req.response().headers().put("Set-Cookie", collection.mutable.Set(urlEncode(key) + "=" + urlEncode(value)))
        sendReply(req, nextReply)
      case Header(key, value, nextReply) =>
        req.response().headers().put(key, collection.mutable.Set(urlEncode(value)))
        sendReply(req, nextReply)
      case x: SyncReply => endResponse(req.response(), x)
    }
  }

  override final def apply(req: HttpServerRequest): Unit = {
    logger.info(s"got a request:${req.path()}")

    val reply = try {
      val path = req.path()
      val routeMatch: RouteMatch = req.method() match {
        case "GET" => Get(path)
        case "PUT" => Put(path)
        case "POST" => Post(path)
        case "DELETE" => Delete(path)
        case "OPTIONS" => Options(path)
        case "HEAD" => Head(path)
        case "TRACE" => Trace(path)
        case "PATCH" => Patch(path)
        case "CONNECT" => Connect(path)
      }
      matcherFor(routeMatch, req)
    } catch {
      case ex: RouterException =>
        errorReplyFromException(ex)
      case ex: Throwable =>
        logger.warn(s"Uncaught Exception for request ${req.absoluteURI()}", ex)
        errorReplyFromException(routerException(ex))
    }

    sendReply(req, reply)
  }

  private def routerException(ex: Throwable): RouterException = ex match {
    case x: RouterException => x
    case x => RouterException(message = x.getMessage, cause = x.getCause)
  }

  protected def errorReplyFromException(ex: RouterException) =
    Error(ex)


  /* TODO
   * error pages -> case Status404 => ?
   */
  private val notFoundFile = "404.html"

  /**
   * Override this method to define the routes of the request handler.
   *
   * @param req The HttpServerRequest that came in.
   * @return A partial function that matches all routes.
   */
  def routes(req: HttpServerRequest): PartialFunction[RouteMatch, Reply]

}