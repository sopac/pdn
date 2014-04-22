package models

import java.util.{ Date }
import play.api.db._
import play.api.Play.current
import anorm._
import anorm.SqlParser._
import scala.language.postfixOps

case class Wiki(id: Pk[Int], expirydate: Option[Date], contenten: Option[String], titleen: Option[String], placeholder: Option[Int], contenttype: Option[Int], contentfr: Option[String], publishdate: Option[Date], externallink: Option[String], titlefr: Option[String], image: Option[Array[Byte]], tags: Option[String])

object Wiki {

  implicit def rowToByteArray: Column[Array[Byte]] = {
    Column.nonNull[Array[Byte]] { (value, meta) =>
      val MetaDataItem(qualified, nullable, clazz) = meta
      value match {
        case bytes: Array[Byte] => Right(bytes)
        case _ => Left(TypeDoesNotMatch("..."))
      }
    }
  }

  def showWiki(id: Int): Wiki = DB.withConnection {
    implicit connection =>
      val sql: SqlQuery = SQL("select * from wiki where id=" + id)
      sql().map(row =>
        Wiki(row[Pk[Int]]("id"), row[Option[Date]]("expirydate"), row[Option[String]]("contenten"), row[Option[String]]("titleen"), row[Option[Int]]("placeholder"), row[Option[Int]]("contenttype"), row[Option[String]]("contentfr"), row[Option[Date]]("publishdate"), row[Option[String]]("externallink"), row[Option[String]]("titlefr"), row[Option[Array[Byte]]]("image"), row[Option[String]]("tags"))).toList.head
  }

  def listNews(num: Int): List[Wiki] = DB.withConnection {
    implicit connection =>
      val sql: SqlQuery = SQL("select * from wiki where contenttype=1 order by placeholder ASC LIMIT " + num)
      sql().map(row =>
        Wiki(row[Pk[Int]]("id"), row[Option[Date]]("expirydate"), row[Option[String]]("contenten"), row[Option[String]]("titleen"), row[Option[Int]]("placeholder"), row[Option[Int]]("contenttype"), row[Option[String]]("contentfr"), row[Option[Date]]("publishdate"), row[Option[String]]("externallink"), row[Option[String]]("titlefr"), row[Option[Array[Byte]]]("image"), row[Option[String]]("tags"))).toList
  }

  def showFocus: Wiki = DB.withConnection {
    implicit connection =>
      val sql: SqlQuery = SQL("select * from wiki where contenttype=2 order by id desc limit 1")
      sql().map(row =>
        Wiki(row[Pk[Int]]("id"), row[Option[Date]]("expirydate"), row[Option[String]]("contenten"), row[Option[String]]("titleen"), row[Option[Int]]("placeholder"), row[Option[Int]]("contenttype"), row[Option[String]]("contentfr"), row[Option[Date]]("publishdate"), row[Option[String]]("externallink"), row[Option[String]]("titlefr"), row[Option[Array[Byte]]]("image"), row[Option[String]]("tags"))).toList.head
  }

  def listInitiatives: List[Wiki] = DB.withConnection {
    implicit connection =>
      val sql: SqlQuery = SQL("select * from wiki where contenttype=3 order by placeholder asc limit 3")
      sql().map(row =>
        Wiki(row[Pk[Int]]("id"), row[Option[Date]]("expirydate"), row[Option[String]]("contenten"), row[Option[String]]("titleen"), row[Option[Int]]("placeholder"), row[Option[Int]]("contenttype"), row[Option[String]]("contentfr"), row[Option[Date]]("publishdate"), row[Option[String]]("externallink"), row[Option[String]]("titlefr"), row[Option[Array[Byte]]]("image"), row[Option[String]]("tags"))).toList
  }

}