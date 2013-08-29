/*
 * Copyright 2011-2013 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.vertx.scala.core.net

import org.vertx.scala.core.FunctionConverters._
import org.vertx.java.core.Handler
import org.vertx.java.core.net.{ NetSocket => JNetSocket }

/**
 * @author swilliams
 * @author <a href="http://www.campudus.com/">Joern Bernhardt</a>
 */
trait ConnectionConfigurer[T] {

  def keyStorePassword(): String

  def keyStorePassword(keyStorePassword: String): T

  def keyStorePath(): String

  def keyStorePath(keyStorePath: String): T

  def receiveBufferSize(): Int

  def receiveBufferSize(receiveBufferSize: Int): T

  def sendBufferSize(): Int

  def sendBufferSize(sendBufferSize: Int): T

  def trafficClass(): Int

  def trafficClass(trafficClass: Int): T

  def trustStorePassword(): String

  def trustStorePassword(password: String): T

  def trustStorePath(): String

  def trustStorePath(path: String): T

}
