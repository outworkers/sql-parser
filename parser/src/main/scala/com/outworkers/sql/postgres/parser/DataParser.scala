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

import com.github.mauricio.async.db.RowData

import scala.util.control.NoStackTrace

/** Result cursor */
trait PostgresCursor {
  /** Current row */
  def row: RowData

  /** Cursor to next row */
  def next: Option[PostgresCursor]

  override lazy val toString = s"Cursor($row)"
}


object PostgresCursor {
  def build(source: IndexedSeq[RowData]): Option[PostgresCursor] = {
    if (source.isEmpty) {
      None
    } else {
      Some {
        new PostgresCursor {
          /** Cursor to next row */
          override def next: Option[PostgresCursor] = build(source.tail)

          /** Current row */
          override def row: RowData = source.head
        }
      }
    }
  }
}

/**
  * Error raised when the specified `column` cannot be found in results.
  *
  * @param column the name of the not found column
  * @param possibilities the names of the available columns
  */
case class ColumnNotFound(
  column: String,
  possibilities: List[String]
) extends SqlRequestError(
  column,
  new RuntimeException(column) with NoStackTrace
) {

  lazy val message: String = s"'$column' not found, available columns: " +
    possibilities.map(_.dropWhile(_ == '.')).mkString(", ")

  override lazy val toString: String = message
}
case class SqlMappingError(reason: String) extends SqlRequestError(
  reason,
  new RuntimeException(reason) with NoStackTrace
) {
  lazy val message = s"SqlMappingError($reason)"
  override lazy val toString: String = message
}

case class UnexpectedNullableFound(reason: String) extends SqlRequestError(
  reason,
  new RuntimeException(reason) with NoStackTrace
) {
  lazy val message = s"UnexpectedNullableFound($reason)"
  override lazy val toString: String = message
}

case class TypeDoesNotMatch(reason: String) extends SqlRequestError(
  reason,
  new RuntimeException(reason) with NoStackTrace
){
  lazy val message = s"TypeDoesNotMatch($reason)"
  override lazy val toString: String = message
}

object DataParser {

  /**
    * Returns a parser that fold over the row.
    *
    * {{{
    * val p: RowParser[List[(Any, String)]] =
    *   SqlParser.folder(List.empty[(Any, String)]) { (ls, v, m) =>
    *     Right((v, m.clazz) :: ls)
    *   }
    * }}}
    */
  def folder[T](z: T)(f: (T, Any) => Either[SqlRequestError, T]): PostgresRowParser[T] = {
    @annotation.tailrec
    def go(data: RowData, index: Int, out: T): ParserResult[T] =

      if (data.isDefinedAt(index)) {
        f(out, data(index)) match {
          case Left(err) => ParseError(err)
          case Right(_) => go(data, index + 1, out)
        }
      } else {
        ParseSuccess(out)
      }

    PostgresRowParser[T] { row: RowData => go(row, 0, z) }
  }

  /**
    * Flatten columns tuple-like.
    *
    * {{{
    * import anorm.SQL
    * import anorm.SqlParser.{ long, str, int }
    *
    * val tuple: (Long, String, Int) = SQL("SELECT a, b, c FROM Test").
    *   as(long("a") ~ str("b") ~ int("c") map (SqlParser.flatten) single)
    * }}}
    */
  def flatten[T1, T2, R](implicit f: TupleFlattener[(T1 ~ T2) => R]): ((T1 ~ T2) => R) = f.fn

  /**
    * Parses specified array column as list.
    *
    * {{{
    * import anorm.{ SQL, SqlParser }
    *
    * val t: (String, List[String]) = SQL("SELECT a, sqlArrayOfString FROM test")
    *   .as(SqlParser.str("a") ~ SqlParser.list("sqlArrayOfString") map (
    *     SqlParser.flatten) single)
    * }}}
    */
  def list[T](columnName: String)(
    implicit c: DataType[List[T]]
  ): PostgresRowParser[List[T]] = get[List[T]](columnName)(c)

  /**
    * Parses specified array column as list.
    * @param columnPosition from 1 to n
    *
    * {{{
    * import anorm.{ SQL, SqlParser }
    *
    * val t: (String, List[String]) = SQL("SELECT a, sqlArrayOfString FROM test")
    *   .as(SqlParser.str("a") ~ SqlParser.list(2) map (
    *     SqlParser.flatten) single)
    * }}}
    */
  def list[T](columnPosition: Int)(
    implicit c: DataType[List[T]]
  ): PostgresRowParser[List[T]] = get[List[T]](columnPosition)(c)

  /**
    * Returns row parser for column with given `name`.
    * @param name Column name
    *
    * {{{
    * import anorm.SQL
    * import anorm.SqlParser.get
    *
    * val title: String = SQL("SELECT title FROM Books").
    *   as(get[String]("title").single)
    * }}}
    */
  def get[T](name: String)(implicit extractor: DataType[T]): PostgresRowParser[T] =
  PostgresRowParser { row =>
    (for {
      res <- extractor.parse(row, name)
    } yield res).fold(ex => ParseError(SqlRequestError(ex)), ParseSuccess(_))
  }

  /**
    * Returns row parser for column at given `position`.
    * @param position Column position, from 1 to n
    *
    * {{{
    * import anorm.SQL
    * import anorm.SqlParser.get
    *
    * val res: (Float, String) = // parsing columns #1 & #3
    *   SQL("SELECT * FROM Test").as(get[String](1) ~ get[Float](3) map {
    *     case str ~ f => (f -> str)
    *   } *)
    * }}}
    */
  def get[T](position: Int)(implicit extractor: DataType[T]): PostgresRowParser[T] =
  PostgresRowParser { row =>
    {
      for {
        res <- extractor.parse(row, position - 1)
      } yield res
    }.fold(e => ParseError(SqlRequestError(e)), a => ParseSuccess(a))
  }

  /**
    * Returns row parser which true if specified `column` is found
    * and matching expected `value`.
    *
    * {{{
    * import anorm.SQL
    * import anorm.SqlParser.matches
    *
    * val m: Boolean = SQL("SELECT * FROM table").as(matches("a", 1.2f).single)
    * // true if column |a| is found and matching 1.2f, otherwise false
    * }}}
    *
    * @return true if matches, or false if not
    */
  def matches[TT, T <: TT](
    column: String,
    value: T
  )(implicit c: DataType[TT]): PostgresRowParser[Boolean] = get[TT](column)(c).?.map(_.fold(false)(_ == value))

}

/** Columns tuple-like */
// Using List or HList?
final case class ~[+A, +B](_1: A, _2: B)

object PostgresRowParser {

  def apply[A : DataType](name: String): PostgresRowParser[A] = new PostgresRowParser[A] {
    override def apply(row: RowData): ParserResult[A] = DataType[A].parse(row, name)
  }

  def apply[A](f: RowData => ParserResult[A]): PostgresRowParser[A] = new PostgresRowParser[A] {
    def apply(row: RowData): ParserResult[A] = f(row)
  }

  /** Row parser that result in successfully unchanged row. */
  object successful extends PostgresRowParser[RowData] {
    def apply(row: RowData): ParserResult[RowData] = ParseSuccess(row)
  }

  def failed[A](error: => ParseError): PostgresRowParser[A] = new PostgresRowParser[A] {
    def apply(row: RowData): ParserResult[A] = error
  }
}

trait PostgresRowParser[+A] extends (RowData => ParserResult[A]) { parent =>

  /**
    * Returns a parser that will apply given function `f`
    * to the result of this first parser. If the current parser is not
    * successful, the new one will return encountered [[Error]].
    *
    * @param f Function applied on the successful parser result
    *
    * {{{
    * import anorm.{ RowParser, SQL, SqlParser }
    *
    * val parser: RowParser[Int] = SqlParser.str("col").map(_.length)
    * // Prepares a parser that first get 'col' string value,
    * // and then returns the length of that
    * }}}
    */
  def map[B](f: A => B): PostgresRowParser[B] = PostgresRowParser(parent.andThen(_.map(f)))

  /**
    * Returns parser which collects information
    * from already parsed row data using `f`.
    *
    * @param otherwise Message returned as error if nothing can be collected using `f`.
    * @param f Collecting function
    */
  def collect[B](otherwise: String)(f: PartialFunction[A, B]): PostgresRowParser[B] =
  PostgresRowParser(parent(_).flatMap(
    f.lift(_).fold[ParserResult[B]](ParseError(SqlMappingError(otherwise)))(ParseSuccess(_)))
  )

  def flatMap[B](k: A => PostgresRowParser[B]): PostgresRowParser[B] =
    PostgresRowParser(row => parent(row).flatMap(k(_)(row)))

  /**
    * Combines this parser on the left of the parser `p` given as argument.
    *
    * @param p Parser on the right
    *
    * {{{
    * val populations: List[String ~ Int] =
    *   SQL("SELECT * FROM Country").as((str("name") ~ int("population")).*)
    * }}}
    */
  def ~[B](p: PostgresRowParser[B]): PostgresRowParser[A ~ B] =
  PostgresRowParser(row => parent(row).flatMap(a => p(row).map(new ~(a, _))))

  /**
    * Combines this current parser with the one given as argument `p`,
    * if and only if the current parser can first/on left side successfully
    * parse a row, without keeping these values in parsed result.
    *
    * {{{
    * import anorm.{ SQL, SqlParser }, SqlParser.{ int, str }
    *
    * val String = SQL("SELECT * FROM test").
    *   as((int("id") ~> str("val")).single)
    * // row has to have an int column 'id' and a string 'val' one,
    * // keeping only 'val' in result
    * }}}
    */
  def ~>[B](p: PostgresRowParser[B]): PostgresRowParser[B] =
  PostgresRowParser(row => parent(row).flatMap(_ => p(row)))

  /**
    * Combines this current parser with the one given as argument `p`,
    * if and only if the current parser can first successfully
    * parse a row, without keeping the values of the parser `p`.
    *
    * {{{
    * import anorm.{ SQL, SqlParser }, SqlParser.{ int, str }
    *
    * val Int = SQL("SELECT * FROM test").
    *   as((int("id") <~ str("val")).single)
    * // row has to have an int column 'id' and a string 'val' one,
    * // keeping only 'id' in result
    * }}}
    */
  def <~[B](p: PostgresRowParser[B]): PostgresRowParser[A] = parent.~(p).map(_._1)

  // TODO: Scaladoc
  def |[B >: A](p: PostgresRowParser[B]): PostgresRowParser[B] = PostgresRowParser { row =>
    this.apply(row) match {
      case ParseError(_) => p(row)
      case a => a
    }
  }

  /**
    * Returns a row parser for optional column,
    * that will turn missing or null column as None.
    */
  def ? : PostgresRowParser[Option[A]] = PostgresRowParser { row =>
    this.apply(row) match {
      case ParseSuccess(a) => ParseSuccess(Some(a))
      case ParseError(UnexpectedNullableFound(_)) | ParseError(_: ColumnNotFound) =>
        ParseSuccess(None)
      case ParseError(_) => ParseSuccess(Option.empty[A])
    }
  }

  /** Alias for [[flatMap]] */
  def >>[B](f: A => PostgresRowParser[B]): PostgresRowParser[B] = flatMap(f)

  /**
    * Returns possibly empty list parsed from result.
    *
    * {{{
    * val price = 125
    * SQL"SELECT name FROM item WHERE price < \\$price".as(scalar[String].*)
    * }}}
    */
  def * : ResultSetParser[List[A]] = ResultSetParser.list(parent)

  /**
    * Returns non empty list parse from result,
    * or raise error if there is no result.
    *
    * {{{
    * import anorm.SQL
    * import anorm.SqlParser.str
    *
    * val parser = str("title") ~ str("descr")
    * SQL("SELECT title, descr FROM pages").as(parser.+) // at least 1 page
    * }}}
    */
  def + : ResultSetParser[List[A]] = ResultSetParser.nonEmptyList(parent)

  /**
    * Returns a result set parser expecting exactly one row to parse.
    *
    * {{{
    * val b: Boolean = SQL("SELECT flag FROM Test WHERE id = :id").
    *   on("id" -> 1).as(scalar[Boolean].single)
    * }}}
    *
    * @see #singleOpt
    */
  def single = ResultSetParser.single(parent)

  /**
    * Returns a result set parser for none or one parsed row.
    *
    * {{{
    * val name: Option[String] =
    *   SQL("SELECT name FROM Country WHERE lang = :lang")
    *   .on("lang" -> "notFound").as(scalar[String].singleOpt)
    * }}}
    */
  def singleOpt: ResultSetParser[Option[A]] = ResultSetParser.singleOpt(parent)

}

/** Parses result from the cursor. */
sealed trait ResultSetParser[+A] extends (Option[PostgresCursor] => ParserResult[A]) {
  parent =>
  def map[B](f: A => B): ResultSetParser[B] = ResultSetParser(parent(_).map(f))
}

private[postgres] object ResultSetParser {
  def apply[A](f: Option[PostgresCursor] => ParserResult[A]): ResultSetParser[A] =
    new ResultSetParser[A] { cur =>
      def apply(cur: Option[PostgresCursor]): ParserResult[A] = f(cur)
    }

  def list[A](p: PostgresRowParser[A]): ResultSetParser[List[A]] = {
    // Performance note: sequence produces a List in reverse order, since appending to a
    // List is an O(n) operation, and this is done n times, yielding O(n2) just to convert the
    // result set to a List.  Prepending is O(1), so we use prepend, and then reverse the result
    // in the map function below.
    @annotation.tailrec
    def sequence(results: List[A], cur: Option[PostgresCursor]): ParserResult[List[A]] =
    cur match {
      case Some(c) => p(c.row) match {
        case ParseSuccess(a) => sequence(a :: results, c.next)
        case ParseError(msg) => ParseError(msg)
      }
      case _ => ParseSuccess(results.reverse)
    }

    ResultSetParser { c => sequence(List.empty[A], c) }
  }

  def nonEmptyList[A](p: PostgresRowParser[A]): ResultSetParser[List[A]] =
    ResultSetParser(rows =>
      if (rows.isEmpty) ParseError(SqlMappingError("Empty Result Set"))
      else list(p)(rows))

  def single[A](p: PostgresRowParser[A]): ResultSetParser[A] = ResultSetParser {
    case Some(cur) if cur.next.isEmpty => p(cur.row)
    case None => ParseError(SqlMappingError("No rows when expecting a single one"))
    case _ => ParseError(SqlMappingError(
      "too many rows when expecting a single one"))

  }

  def singleOpt[A](p: PostgresRowParser[A]): ResultSetParser[Option[A]] =
    ResultSetParser {
      case Some(cur) if cur.next.isEmpty => p.map(Some(_))(cur.row)
      case None => ParseSuccess(Option.empty[A])
      case _ => ParseError(SqlMappingError("Too many rows when expecting a single one"))
    }

}