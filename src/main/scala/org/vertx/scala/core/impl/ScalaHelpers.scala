package org.vertx.scala.core.impl

import scala.concurrent.{Future, Promise}
import org.vertx.scala.core._
import org.vertx.scala.core.FunctionConverters._
import scala.util.{Try, Failure, Success}

/**
 * General helpers for generic scala problems that occur inside the Vert.x code.
 *
 * @author <a href="http://www.campudus.com/">Joern Bernhardt</a>
 */
object ScalaHelpers {

  type ResultHandler[T] = AsyncResult[T] => Unit

  def promisify[T](x: Promise[T] => _): Future[T] = {
    val p = Promise[T]()
    x(p)
    p.future
  }

  def asyncResultPromisify[T](fn: ResultHandler[T] => _): Future[T] = promisify { p: Promise[T] =>
    val t = {
      case Success(result) => p.success(result)
      case Failure(ex) => p.failure(ex)
    }: Try[T] => Unit
    fn(t)
  }

}
