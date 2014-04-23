package models

import java.util.{ Date }
import play.api.db._
import play.api.Play.current
import anorm._
import anorm.SqlParser._
import scala.language.postfixOps
import scala.collection.mutable.ListBuffer

case class Calendar(id: Pk[Int], location: Option[String], endtime: Option[String], calendaryear: Option[Int], latitude: Option[Double], link: Option[String], description: Option[String], invitationonly: Option[Boolean], contacts: Option[String], starttime: Option[String], comments: Option[String], title: Option[String], country_iso: Option[String], enddate: Option[Date], longitude: Option[Double], eventtype: Option[Int], startdate: Option[Date], country_name: Option[String], country_id: Option[Int], venue: Option[String])

case class CalendarLink(id: Pk[Int], link: Option[String], description: Option[String])

case class CalendarOrganisation(id: Pk[Int], organisation_id: Option[Int], name: Option[String], acronym: Option[String])

object Calendar {

  val types = Map(0 -> "Unknown", 1 -> "Meeting", 2 -> "Training", 3 -> "Conference", 4 -> "Workshop", 5 -> "Other")

  def getFilterList(col: String): List[String] = DB.withConnection {
    implicit connection =>
      var str: String = "SELECT DISTINCT " + col + " FROM public.calendar AS calendar LEFT OUTER JOIN public.country AS country ON calendar.country_id = country.id LEFT OUTER JOIN public.calendarlink AS calendarlink ON calendarlink.calendar_id = calendar.id ORDER BY " + col
      var resList = new ListBuffer[String]()

      if (col.equals("eventtype")) {
        for ((k, v) <- types) { resList += v }
      } else {
        val sql: SqlQuery = SQL(str)
        sql().toList.map { r =>
          r.data.map { d =>
            if (d != null) resList += d.toString
          }
        }
      }

      //remove duplicates
      resList = resList.distinct
      //sort
      resList = resList.sorted
      return resList.toList
  }

  def showCalendar(id: Int): Calendar = DB.withConnection {
    implicit connection =>
      val sql: SqlQuery = SQL("SELECT calendar.*, country.name AS country_name, country.iso AS country_iso, calendarlink.link, calendarlink.description FROM public.calendar AS calendar LEFT OUTER JOIN public.country AS country ON calendar.country_id = country.id LEFT OUTER JOIN public.calendarlink AS calendarlink ON calendarlink.calendar_id = calendar.id WHERE calendar.id=" + id)
      sql().map(row =>
        Calendar(row[Pk[Int]]("id"), row[Option[String]]("location"), row[Option[String]]("endtime"), row[Option[Int]]("calendaryear"), row[Option[Double]]("latitude"), row[Option[String]]("link"), row[Option[String]]("description"), row[Option[Boolean]]("invitationonly"), row[Option[String]]("contacts"), row[Option[String]]("starttime"), row[Option[String]]("comments"), row[Option[String]]("title"), row[Option[String]]("country_iso"), row[Option[Date]]("enddate"), row[Option[Double]]("longitude"), row[Option[Int]]("eventtype"), row[Option[Date]]("startdate"), row[Option[String]]("country_name"), row[Option[Int]]("country_id"), row[Option[String]]("venue"))).toList.head
  }

  def listCalendar(page: Int, num: Int, sort: String, sortType: String, filter: String): List[Calendar] = DB.withConnection {
    implicit connection =>
      var str = "SELECT calendar.*, country.name AS country_name, country.iso AS country_iso, calendarlink.link, calendarlink.description FROM public.calendar AS calendar LEFT OUTER JOIN public.country AS country ON calendar.country_id = country.id LEFT OUTER JOIN public.calendarlink AS calendarlink ON calendarlink.calendar_id = calendar.id ORDER BY " + sort + " " + sortType + " LIMIT " + num + " OFFSET " + (page * num).toString()
      //filter
      if (!filter.equals("")) {
        val arr = filter.split(":")
        val col = arr(0).trim()
        val value = arr(1).trim()

        if (col.equals("eventtype")) {
          str = "SELECT calendar.*, country.name AS country_name, country.iso AS country_iso, calendarlink.link, calendarlink.description FROM public.calendar AS calendar LEFT OUTER JOIN public.country AS country ON calendar.country_id = country.id LEFT OUTER JOIN public.calendarlink AS calendarlink ON calendarlink.calendar_id = calendar.id WHERE calendar.eventtype=" + (types.valuesIterator.toList.indexOf(value) - 1) + " ORDER BY " + sort + " " + sortType + " LIMIT " + num + " OFFSET " + (page * num).toString()
        }

        if (col.equals("country.name")) {
          str = "SELECT calendar.*, country.name AS country_name, country.iso AS country_iso, calendarlink.link, calendarlink.description FROM public.calendar AS calendar LEFT OUTER JOIN public.country AS country ON calendar.country_id = country.id LEFT OUTER JOIN public.calendarlink AS calendarlink ON calendarlink.calendar_id = calendar.id WHERE country.name='" + value + "' ORDER BY " + sort + " " + sortType + " LIMIT " + num + " OFFSET " + (page * num).toString()
        }

        if (col.equals("calendaryear")) {
          str = "SELECT calendar.*, country.name AS country_name, country.iso AS country_iso, calendarlink.link, calendarlink.description FROM public.calendar AS calendar LEFT OUTER JOIN public.country AS country ON calendar.country_id = country.id LEFT OUTER JOIN public.calendarlink AS calendarlink ON calendarlink.calendar_id = calendar.id WHERE calendar.calendaryear=" + value + " ORDER BY " + sort + " " + sortType + " LIMIT " + num + " OFFSET " + (page * num).toString()
        }

      }

      val sql: SqlQuery = SQL(str)
      sql().map(row =>
        Calendar(row[Pk[Int]]("id"), row[Option[String]]("location"), row[Option[String]]("endtime"), row[Option[Int]]("calendaryear"), row[Option[Double]]("latitude"), row[Option[String]]("link"), row[Option[String]]("description"), row[Option[Boolean]]("invitationonly"), row[Option[String]]("contacts"), row[Option[String]]("starttime"), row[Option[String]]("comments"), row[Option[String]]("title"), row[Option[String]]("country_iso"), row[Option[Date]]("enddate"), row[Option[Double]]("longitude"), row[Option[Int]]("eventtype"), row[Option[Date]]("startdate"), row[Option[String]]("country_name"), row[Option[Int]]("country_id"), row[Option[String]]("venue"))).toList
  }

  def totalCalendars(): Int = DB.withConnection {
    implicit connection =>
      val count: Long = SQL("select count(*) from calendar").as(scalar[Long].single)
      return count.toInt
  }

  def recentCalendar(num: Int): List[Calendar] = DB.withConnection {
    implicit connection =>
      val sql: SqlQuery = SQL("SELECT calendar.*, country.name AS country_name, country.iso AS country_iso, calendarlink.link, calendarlink.description FROM public.calendar AS calendar LEFT OUTER JOIN public.country AS country ON calendar.country_id = country.id LEFT OUTER JOIN public.calendarlink AS calendarlink ON calendarlink.calendar_id = calendar.id ORDER BY calendar.id LIMIT " + num)
      sql().map(row =>
        Calendar(row[Pk[Int]]("id"), row[Option[String]]("location"), row[Option[String]]("endtime"), row[Option[Int]]("calendaryear"), row[Option[Double]]("latitude"), row[Option[String]]("link"), row[Option[String]]("description"), row[Option[Boolean]]("invitationonly"), row[Option[String]]("contacts"), row[Option[String]]("starttime"), row[Option[String]]("comments"), row[Option[String]]("title"), row[Option[String]]("country_iso"), row[Option[Date]]("enddate"), row[Option[Double]]("longitude"), row[Option[Int]]("eventtype"), row[Option[Date]]("startdate"), row[Option[String]]("country_name"), row[Option[Int]]("country_id"), row[Option[String]]("venue"))).toList
  }

  def upcomingCalendar(num: Int): List[Calendar] = DB.withConnection {
    implicit connection =>
      val sql: SqlQuery = SQL("SELECT calendar.*, country.name AS country_name, country.iso AS country_iso, calendarlink.link, calendarlink.description FROM public.calendar AS calendar LEFT OUTER JOIN public.country AS country ON calendar.country_id = country.id LEFT OUTER JOIN public.calendarlink AS calendarlink ON calendarlink.calendar_id = calendar.id WHERE calendar.startdate > now()::date - 365 ORDER BY calendar.startdate ASC LIMIT " + num)
      sql().map(row =>
        Calendar(row[Pk[Int]]("id"), row[Option[String]]("location"), row[Option[String]]("endtime"), row[Option[Int]]("calendaryear"), row[Option[Double]]("latitude"), row[Option[String]]("link"), row[Option[String]]("description"), row[Option[Boolean]]("invitationonly"), row[Option[String]]("contacts"), row[Option[String]]("starttime"), row[Option[String]]("comments"), row[Option[String]]("title"), row[Option[String]]("country_iso"), row[Option[Date]]("enddate"), row[Option[Double]]("longitude"), row[Option[Int]]("eventtype"), row[Option[Date]]("startdate"), row[Option[String]]("country_name"), row[Option[Int]]("country_id"), row[Option[String]]("venue"))).toList
  }

}

object CalendarLink {
  def getCalendarLinks(id: Int): List[CalendarLink] = DB.withConnection {
    implicit connection =>
      val sql: SqlQuery = SQL("SELECT calendar.id, calendarlink.description, calendarlink.link FROM public.calendarlink AS calendarlink RIGHT OUTER JOIN public.calendar AS calendar ON calendarlink.calendar_id = calendar.id WHERE calendar.id=" + id)
      sql().map(row =>
        CalendarLink(row[Pk[Int]]("id"), row[Option[String]]("link"), row[Option[String]]("description"))).toList
  }
}

object CalendarOrganisation {
  def getCalendarOrganisations(id: Int): List[CalendarOrganisation] = DB.withConnection {
    implicit connection =>
      val sql: SqlQuery = SQL("SELECT calendar.id, organisations.name, organisations.acronym, organisations.id as organisation_id FROM public.organisations_calendar AS organisations_calendar RIGHT OUTER JOIN public.calendar AS calendar ON organisations_calendar.calendars_id = calendar.id, public.organisations AS organisations WHERE organisations_calendar.organisation_id = organisations.id AND calendar.id=" + id)
      sql().map(row =>
        CalendarOrganisation(row[Pk[Int]]("id"), row[Option[Int]]("organisation_id"), row[Option[String]]("name"), row[Option[String]]("acronym"))).toList
  }
}
