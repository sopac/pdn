package models

import java.util.{ Date }
import play.api.db._
import play.api.Play.current
import anorm._
import anorm.SqlParser._
import scala.language.postfixOps
import java.text.SimpleDateFormat
import scala.collection.mutable.ListBuffer

case class Alert(id: Pk[Int], source_name: Option[String], source_id: Option[Int], daterecieved: Option[Date], alerttype: Option[Int], source_url: Option[String], ignore: Option[Boolean], source_email: Option[String], content: Option[String], subject: Option[String], countries: Option[String], uuid: Option[String])

object Alert {

  def recentAlerts(num: Int): List[Alert] = DB.withConnection {
    implicit connection =>
      val sql: SqlQuery = SQL("SELECT alert.*, alertsource.alerttype, alertsource.email AS source_email, alertsource.name AS source_name, alertsource.url AS source_url FROM public.alert AS alert, public.alertsource AS alertsource WHERE alert.source_id = alertsource.id ORDER BY alert.id desc limit " + num)
      sql().map(row =>
        Alert(row[Pk[Int]]("id"), row[Option[String]]("source_name"), row[Option[Int]]("source_id"), row[Option[Date]]("daterecieved"), row[Option[Int]]("alerttype"), row[Option[String]]("source_url"), row[Option[Boolean]]("ignore"), row[Option[String]]("source_email"), row[Option[String]]("content"), row[Option[String]]("subject"), row[Option[String]]("countries"), row[Option[String]]("uuid"))).toList
  }

  def getFilterList(col: String): List[String] = DB.withConnection {
    implicit connection =>

      var resList = new ListBuffer[String]()

      val sql: SqlQuery = SQL("SELECT distinct " + col + " FROM public.alert AS alert, public.alertsource AS alertsource WHERE alert.source_id = alertsource.id ORDER BY " + col)
      if (col.equals("alertsource.name")) {
        resList = sql.as(str(col) *).to[ListBuffer]
      }

      //if year
      if (col.equals("alert.daterecieved")) {
        val sdf: SimpleDateFormat = new SimpleDateFormat("yyyy MMMM")
        var dl = new ListBuffer[Date]
        sql().toList.map { r =>
          dl += r[Option[Date]]("daterecieved").get
        }
        dl.map { d =>
          resList += sdf.format(d)
        }

      }

      //if countries
      if (col.equals("alert.countries")) {
        var cList = new ListBuffer[String]()
        cList = sql.as(str(col) *).to[ListBuffer]
        cList.map { cl =>
          cl.split(";").map { c =>
            resList += c
          }
        }
      }

      //remove duplicates
      resList = resList.distinct
      //sort
      resList = resList.sorted

      return resList.toList
  }

  def totalAlerts(): Int = DB.withConnection {
    implicit connection =>
      val count: Long = SQL("select count(*) from alert").as(scalar[Long].single)
      return count.toInt
  }

  def listAlerts(page: Int, num: Int, sort: String, sortType: String, filter: String): List[Alert] = DB.withConnection {
    implicit connection =>

      var str: String = "SELECT alert.*, alertsource.alerttype, alertsource.email AS source_email, alertsource.name AS source_name, alertsource.url AS source_url FROM public.alert AS alert, public.alertsource AS alertsource WHERE alert.source_id = alertsource.id ORDER BY " + sort + " " + sortType + " LIMIT " + num + " OFFSET " + (page * num).toString()

      //filter
      if (!filter.equals("")) {
        val arr = filter.split(":")
        val col = arr(0)
        val value = arr(1)
        if (col.equals("alertsource.name"))
          str = "SELECT alert.*, alertsource.alerttype, alertsource.email AS source_email, alertsource.name AS source_name, alertsource.url AS source_url FROM public.alert AS alert, public.alertsource AS alertsource WHERE alert.source_id = alertsource.id AND " + col + "='" + value + "' ORDER BY alert.daterecieved desc"
        if (col.equals("alert.countries"))
          str = "SELECT alert.*, alertsource.alerttype, alertsource.email AS source_email, alertsource.name AS source_name, alertsource.url AS source_url FROM public.alert AS alert, public.alertsource AS alertsource WHERE alert.source_id = alertsource.id AND " + col + " LIKE '" + value + ";' ORDER BY alert.daterecieved desc"
        //println(str)
      }

      val sql: SqlQuery = SQL(str)
      sql().map(row =>
        Alert(row[Pk[Int]]("id"), row[Option[String]]("source_name"), row[Option[Int]]("source_id"), row[Option[Date]]("daterecieved"), row[Option[Int]]("alerttype"), row[Option[String]]("source_url"), row[Option[Boolean]]("ignore"), row[Option[String]]("source_email"), row[Option[String]]("content"), row[Option[String]]("subject"), row[Option[String]]("countries"), row[Option[String]]("uuid"))).toList
  }

  def showAlert(id: Int): Alert = DB.withConnection {
    implicit connection =>
      val sql: SqlQuery = SQL("SELECT alert.*, alertsource.alerttype, alertsource.email AS source_email, alertsource.name AS source_name, alertsource.url AS source_url FROM public.alert AS alert, public.alertsource AS alertsource WHERE alert.source_id = alertsource.id and alert.id=" + id)
      sql().map(row =>
        Alert(row[Pk[Int]]("id"), row[Option[String]]("source_name"), row[Option[Int]]("source_id"), row[Option[Date]]("daterecieved"), row[Option[Int]]("alerttype"), row[Option[String]]("source_url"), row[Option[Boolean]]("ignore"), row[Option[String]]("source_email"), row[Option[String]]("content"), row[Option[String]]("subject"), row[Option[String]]("countries"), row[Option[String]]("uuid"))).toList.head
  }
}