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
package anorm

import com.github.mauricio.async.db.{Connection, QueryResult}

import scala.concurrent.Future

case class PrebuiltPreparedStatement(
  query: String,
  tokens: Map[String, Any]
)(connection: Connection) {
  def onParams(pairs: (String, Any)*): PrebuiltPreparedStatement = {
    copy(tokens = tokens ++ pairs)(connection)
  }

  //@deprecated("This thing doens't work", "Prepared")
  def on(pairs: NamedParameter*): PrebuiltPreparedStatement = {
    val mapped = pairs.map(param => param.name -> param.value.show)

    copy(tokens = tokens ++ mapped)(connection)
  }

  def flatten(input: Any): Any = {
    if (input.isInstanceOf[Option[_]]) {
      input match {
        case value: Some[_] =>
          value.get
        case _ =>
          null
      }
    } else {
      input
    }
  }

  private[this] def bind(names: List[String]): Seq[Any] = {
    names.flatMap { name =>
      tokens.find(_._1 == name).map {
        case (_, value) => flatten(value)
      }
    }
  }

  def execute(): Future[QueryResult] = {
    SqlStatementParser.parse(query) match {
      case scala.util.Success(ts) =>

        val query = SqlQuery.prepare(ts, ts.names).statement

        val bindables = bind(ts.names)

        connection.sendPreparedStatement(
          query,
          bindables
        )

      case f @ scala.util.Failure(err) => {
        Console.println("Is this not supporeted or what")
        Console.println(err.getMessage)
        Future.failed(err)
      }
    }

  }
}
