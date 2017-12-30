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

import java.util.UUID

import com.github.mauricio.async.db.RowData
import com.github.mauricio.async.db.general.ArrayRowData
import org.joda.time.DateTime

import scala.util.Try


trait DataType[T] {
  def parse(row: RowData, name: String): ParserResult[T] = {

    if (Option(row.apply(name)).isEmpty) {
      Console.println(s"Unable to locate column $name, row only containes")
      Console.println(row.asInstanceOf[ArrayRowData].columns.mkString("\n"))
    }

    ParserResult(row(name).asInstanceOf[T])
  }

  def parse(row: RowData, index: Int): ParserResult[T] = {
    ParserResult(row(index).asInstanceOf[T])
  }
}

object DataType {

  implicit object StringDataType extends DataType[String] {
    override def parse(row: RowData, name: String): ParserResult[String] = {
      ParserResult(row(name).toString)
    }
  }

  implicit object IntDataType extends DataType[Int] {
    override def parse(row: RowData, name: String): ParserResult[Int] = {
      ParserResult(Integer.parseInt(row(name).toString))
    }
  }

  implicit object DoubleDataType extends DataType[Double] {
    override def parse(row: RowData, name: String): ParserResult[Double] = {
      ParserResult(Double.unbox(row(name).asInstanceOf[AnyRef]))
    }
  }

  implicit object LongDataType extends DataType[Long] {
    override def parse(row: RowData, name: String): ParserResult[Long] = {
      ParserResult(Long.unbox(row(name).asInstanceOf[AnyRef]))
    }
  }

  implicit object ShortDataType extends DataType[Short] {
    override def parse(row: RowData, name: String): ParserResult[Short] = {
      ParserResult(Short.unbox(row(name).asInstanceOf[AnyRef]))
    }
  }

  implicit object FloatDataType extends DataType[Float] {
    override def parse(row: RowData, name: String): ParserResult[Float] = {
      ParserResult(Float.unbox(row(name).asInstanceOf[AnyRef]))
    }
  }


  implicit object JodaDateTime extends DataType[DateTime] {
    override def parse(row: RowData, name: String): ParserResult[DateTime] = {
      ParserResult(row(name).asInstanceOf[org.joda.time.DateTime])
    }
  }

  implicit val uuidParser: DataType[UUID] = new DataType[UUID] {
    override def parse(row: RowData, name: String): ParserResult[UUID] = {
      ParserResult(Try(UUID.fromString(row(name).toString)))
    }
  }

  def apply[T](implicit ev: DataType[T]): DataType[T] = ev

  def derive[S, T](fn: S => T)(
    implicit ev: DataType[S]
  ): DataType[T] = new DataType[T] {
    override def parse(row: RowData, name: String): ParserResult[T] = {
      ev.parse(row, name) map fn
    }
  }

  implicit def auto[T]: DataType[T] = new DataType[T] {}

  implicit def autoOpt[T]: DataType[Option[T]] = new DataType[Option[T]] {
    override def parse(row: RowData, name: String): ParserResult[Option[T]] = {
      Option(row(name)) match {
        case Some(value) => ParserResult(Try(value.asInstanceOf[T]) map Some.apply)
        case None => ParseSuccess(None)
      }
    }
  }

}