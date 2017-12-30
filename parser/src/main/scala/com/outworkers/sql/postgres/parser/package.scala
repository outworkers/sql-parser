package com.outworkers.sql.postgres


import java.time.{Instant, ZoneId, ZonedDateTime}

import anorm.PrebuiltPreparedStatement
import com.github.mauricio.async.db.{Connection, QueryResult, RowData}
import com.outworkers.sql.postgres.parser.{DataType, ParserResult, PostgresRowParser}
import org.joda.time.DateTime

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

package object postgres {

  implicit class ConnectionAugmenter(val connection: Connection) extends AnyVal  {
    def prepare(query: String): PrebuiltPreparedStatement = {
      PrebuiltPreparedStatement(query, Map.empty)(connection)
    }
  }

  def scalar[T <: AnyVal](implicit transformer: DataType[T]): PostgresRowParser[T] =
    new PostgresRowParser[T] {
      def apply(row: RowData): ParserResult[T] = {
        transformer.parse(row, 0)
      }
    }

  implicit class OptRowAugmenter(val data: Option[RowData]) extends AnyVal {
    def as[T : Parser]: Try[T] = for (dt <- data.asTry; parse <- Parser[T].apply(dt)) yield parse
  }

  implicit class FutureQueryAug(val source: Future[QueryResult]) extends AnyVal {

    def asBool(implicit ec: ExecutionContext): Future[Boolean] = {
      source map (_.rowsAffected > 0)
    }

    def asOpt[T](implicit parser: PostgresRowParser[T], ec: ExecutionContext): Future[Option[T]] = {
      source flatMap { result =>
        result.rows match {
          case Some(resultSet) =>
            if (resultSet.nonEmpty) {
              parser(resultSet.head) match {
                case ParseSuccess(value) => Future.successful(Some(value))
                case ParseError(err) => Future.failed(err)
              }
            } else {
              Future.successful(None)
            }
          case None => Future.successful(None)
        }
      }
    }

    def queryResultToCursor(res: QueryResult): Option[PostgresCursor] = {
      res.rows flatMap PostgresCursor.build
    }

    def as[T](parser: ResultSetParser[T])(implicit ec: ExecutionContext): Future[T] = {
      source map (rows => parser(queryResultToCursor(rows))) flatMap {
        case ParseSuccess(projection) => Future.successful(projection)
        case ParseError(msg) => Future.failed(msg)
      }
    }

    def fetch[T](implicit parser: PostgresRowParser[T], ec: ExecutionContext): Future[IndexedSeq[T]] = {
      source flatMap { result =>
        result.rows match {
          case Some(resultSet) =>
            val futures = resultSet.map { el =>
              parser(el).fold(err => Future.failed(err), Future.successful)
            }

            Future.sequence(futures)
          case None => Future.successful(IndexedSeq.empty[T])
        }
      }
    }
  }

  implicit class OptAug[T](val opt: Option[T]) extends AnyVal {
    def asTry: Try[T] = opt.fold[Try[T]](Failure[T](new RuntimeException("Row data was empty")))(Success(_))
  }

  implicit class RowAugmenter(val data: RowData) extends AnyVal {
    def as[T : Parser]: T = Parser[T].apply(data).get

    def asOpt[T : Parser]: Option[T] = Parser[T].apply(data).toOption
  }

  def get[T : DataType](name: String): PostgresRowParser[T] = PostgresRowParser[T](name)

  def getOpt[T : DataType](name: String): PostgresRowParser[Option[T]] = PostgresRowParser[Option[T]](name)

  def zonedDateTime(columnName: String): PostgresRowParser[ZonedDateTime] = {
    get[DateTime](columnName).map { dt =>
      ZonedDateTime.ofInstant(Instant.ofEpochMilli(dt.getMillis), ZoneId.of("UTC")).withFixedOffsetZone()
    }
  }

  def zonedDateTimeOpt(columnName: String): PostgresRowParser[Option[ZonedDateTime]] = {
    get[Option[ZonedDateTime]](columnName).map(_.map(_.withFixedOffsetZone()))
  }

  def flatten[T1, T2, R](implicit f: TupleFlattener[(T1 ~ T2) => R]): ((T1 ~ T2) => R) = f.fn

}