package models

import java.util.{ Date }
import play.api.db._
import play.api.Play.current
import anorm._
import anorm.SqlParser._
import scala.language.postfixOps

case class Calendar(id: Pk[Int], location: Option[String], endtime: Option[String], calendaryear: Option[Int], latitude: Option[Double], link: Option[String], description: Option[String], invitationonly: Option[Boolean], contacts: Option[String], starttime: Option[String], comments: Option[String], title: Option[String], country_iso: Option[String], enddate: Option[Date], longitude: Option[Double], eventtype: Option[Int], startdate: Option[Date], country_name: Option[String], country_id: Option[Int], venue: Option[String])

object Calendar {

  def listCalendar: List[Calendar] = DB.withConnection {
    implicit connection =>
      val sql: SqlQuery = SQL("SELECT calendar.*, country.name AS country_name, country.iso AS country_iso, calendarlink.link, calendarlink.description FROM public.calendar AS calendar LEFT OUTER JOIN public.country AS country ON calendar.country_id = country.id LEFT OUTER JOIN public.calendarlink AS calendarlink ON calendarlink.calendar_id = calendar.id")
      sql().map(row =>
        Calendar(row[Pk[Int]]("id"), row[Option[String]]("location"), row[Option[String]]("endtime"), row[Option[Int]]("calendaryear"), row[Option[Double]]("latitude"), row[Option[String]]("link"), row[Option[String]]("description"), row[Option[Boolean]]("invitationonly"), row[Option[String]]("contacts"), row[Option[String]]("starttime"), row[Option[String]]("comments"), row[Option[String]]("title"), row[Option[String]]("country_iso"), row[Option[Date]]("enddate"), row[Option[Double]]("longitude"), row[Option[Int]]("eventtype"), row[Option[Date]]("startdate"), row[Option[String]]("country_name"), row[Option[Int]]("country_id"), row[Option[String]]("venue"))).toList
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
