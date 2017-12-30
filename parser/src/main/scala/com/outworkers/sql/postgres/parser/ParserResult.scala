/*
 * Copyright 2017 - 2018 Outworkers Ltd.
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
package com.outworkers.sql.postgres.parser

import scala.util.{Failure, Try}

/** Anorm runtime exception */
final case class SqlException(message: String)
  extends Exception with scala.util.control.NoStackTrace {
  override def getMessage: String = message
}

/** Error from processing SQL */
class SqlRequestError(
  message: String,
  cause: Throwable
) extends RuntimeException(message, cause) {

  /** Returns error as a failure. */
  def toFailure = Failure(SqlException(message))
}

object SqlRequestError {

  def apply(failure: Throwable): SqlRequestError = {
    new SqlRequestError(failure.getMessage, failure)
  }

  def apply[T](failure: Failure[T]): SqlRequestError = {
    new SqlRequestError(failure.exception.getMessage, failure.exception)
  }
}

/** Parsed SQL result. */
sealed trait ParserResult[+A] { self =>

  def toOption: Option[A] = fold(err => Option.empty[A], Some.apply)

  def flatMap[B](k: A => ParserResult[B]): ParserResult[B] = self match {
    case ParseSuccess(a) => k(a)
    case e @ ParseError(_) => e
  }

  def map[B](f: A => B): ParserResult[B] = self match {
    case ParseSuccess(a) => ParseSuccess(f(a))
    case e @ ParseError(_) => e
  }

  /**
    * Either applies function `e` if result is erroneous,
    * or function `f` with successful result if any.
    */
  def fold[B](e: SqlRequestError => B, f: A => B): B = self match {
    case ParseSuccess(a) => f(a)
    case ParseError(err) => e(err)
  }
}

object ParserResult {

  def apply[T](fn: => T): ParserResult[T] = ParserResult(Try(fn))

  def apply[T](source: Try[T]): ParserResult[T] = {
    source.fold(ex => ParseError(SqlRequestError(ex)), ParseSuccess.apply)
  }
}

case class ParseError(msg: SqlRequestError) extends ParserResult[Nothing]
case class ParseSuccess[A](a: A) extends ParserResult[A]