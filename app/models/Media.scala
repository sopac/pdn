package models

import java.util.{ Date }
import play.api.db._
import play.api.Play.current
import anorm._
import anorm.SqlParser._
import scala.language.postfixOps

case class Media(id: Pk[Int], mediatype: Option[Int], thumbnail: Option[Array[Byte]], source: Option[String], filename: Option[String], description: Option[String], filesize: Option[Int], title: Option[String], copyright: Option[String])

object Media {

  implicit def rowToByteArray: Column[Array[Byte]] = {
    Column.nonNull[Array[Byte]] { (value, meta) =>
      val MetaDataItem(qualified, nullable, clazz) = meta
      value match {
        case bytes: Array[Byte] => Right(bytes)
        case _ => Left(TypeDoesNotMatch("..."))
      }
    }
  }

  def listMedia: List[Media] = DB.withConnection {
    implicit connection =>
      val sql: SqlQuery = SQL("select * from media order by id desc")
      sql().map(row =>
        Media(row[Pk[Int]]("id"), row[Option[Int]]("mediatype"), row[Option[Array[Byte]]]("thumbnail"), row[Option[String]]("source"), row[Option[String]]("filename"), row[Option[String]]("description"), row[Option[Int]]("filesize"), row[Option[String]]("title"), row[Option[String]]("copyright"))).toList
  }
}