package models

import java.util.{ Date }
import play.api.db._
import play.api.Play.current
import anorm._
import anorm.SqlParser._
import scala.language.postfixOps

case class Media(id: Pk[Int], mediatype: Option[Int], thumbnail: Option[Array[Byte]], source: Option[String], filename: Option[String], description: Option[String], filesize: Option[Long], title: Option[String], copyright: Option[String])

object Media {

  val mediaType = List("Video", "Photo", "Audio", "Presentation")

  implicit def rowToByteArray: Column[Array[Byte]] = {
    Column.nonNull[Array[Byte]] { (value, meta) =>
      val MetaDataItem(qualified, nullable, clazz) = meta
      value match {
        case bytes: Array[Byte] => Right(bytes)
        case _ => Left(TypeDoesNotMatch("..."))
      }
    }
  }

  def listMedia(page: Int, num: Int, sort: String, sortType: String, filter: String): List[Media] = DB.withConnection {
    implicit connection =>
      var sql: SqlQuery = SQL("select * from media ORDER BY " + sort + " " + sortType + " LIMIT " + num + " OFFSET " + (page * num).toString())

      //filter
      if (!filter.equals("")) {
        val arr = filter.split(":")
        val col = arr(0).trim()
        val value = arr(1).trim()
        if (col.equals("mediatype")) {
          sql = SQL("select * from media WHERE mediatype=" + mediaType.indexOf(value) + " ORDER BY " + sort + " " + sortType + " LIMIT " + num + " OFFSET " + (page * num).toString())
        }
      }

      sql().map(row =>
        Media(row[Pk[Int]]("id"), row[Option[Int]]("mediatype"), row[Option[Array[Byte]]]("thumbnail"), row[Option[String]]("source"), row[Option[String]]("filename"), row[Option[String]]("description"), row[Option[Long]]("filesize"), row[Option[String]]("title"), row[Option[String]]("copyright"))).toList
  }

  def showMedia(id: Int): Media = DB.withConnection {
    implicit connection =>
      val sql: SqlQuery = SQL("select * from media where id=" + id)
      sql().map(row =>
        Media(row[Pk[Int]]("id"), row[Option[Int]]("mediatype"), row[Option[Array[Byte]]]("thumbnail"), row[Option[String]]("source"), row[Option[String]]("filename"), row[Option[String]]("description"), row[Option[Long]]("filesize"), row[Option[String]]("title"), row[Option[String]]("copyright"))).toList.head
  }

  def totalMedia(): Int = DB.withConnection {
    implicit connection =>
      val count: Long = SQL("select count(*) from media").as(scalar[Long].single)
      return count.toInt
  }
}