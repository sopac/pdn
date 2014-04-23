package models

import java.util.{ Date }
import play.api.db._
import play.api.Play.current
import anorm._
import anorm.SqlParser._
import scala.language.postfixOps
import scala.collection.mutable.ListBuffer

case class Contact(id: Pk[Int], url1: Option[String], city: Option[String], designation: Option[String], organisation_id: Option[Int], email2: Option[String], physicaladdress: Option[String], languages: Option[String], voiceoverip: Option[String], telephone2: Option[String], photo: Option[Array[Byte]], department: Option[String], name: Option[String], logo: Option[Array[Byte]], email1: Option[String], url2: Option[String], lastname: Option[String], gender: Option[Int], country_iso: Option[String], contacttype: Option[Int], satellitephone: Option[String], postaladdress: Option[String], firstname: Option[String], fax: Option[String], postcode: Option[String], telephone1: Option[String], country_name: Option[String], satellitephonesystem: Option[Int], tags: Option[String], country_id: Option[Int], acronym: Option[String])

object Contact {
  def listContact: List[Contact] = DB.withConnection {
    implicit connection =>
      val sql: SqlQuery = SQL("SELECT contact.*, organisations.id AS organisation_id, organisations.name, organisations.acronym, organisations.logo, country.name AS country_name, country.iso AS country_iso FROM public.contact AS contact LEFT OUTER JOIN public.organisations AS organisations ON contact.organisation_id = organisations.id LEFT OUTER JOIN public.country AS country ON contact.country_id = country.id order by contact.id")
      sql().map(row =>
        Contact(row[Pk[Int]]("id"), row[Option[String]]("url1"), row[Option[String]]("city"), row[Option[String]]("designation"), row[Option[Int]]("organisation_id"), row[Option[String]]("email2"), row[Option[String]]("physicaladdress"), row[Option[String]]("languages"), row[Option[String]]("voiceoverip"), row[Option[String]]("telephone2"), row[Option[Array[Byte]]]("photo"), row[Option[String]]("department"), row[Option[String]]("name"), row[Option[Array[Byte]]]("logo"), row[Option[String]]("email1"), row[Option[String]]("url2"), row[Option[String]]("lastname"), row[Option[Int]]("gender"), row[Option[String]]("country_iso"), row[Option[Int]]("contacttype"), row[Option[String]]("satellitephone"), row[Option[String]]("postaladdress"), row[Option[String]]("firstname"), row[Option[String]]("fax"), row[Option[String]]("postcode"), row[Option[String]]("telephone1"), row[Option[String]]("country_name"), row[Option[Int]]("satellitephonesystem"), row[Option[String]]("tags"), row[Option[Int]]("country_id"), row[Option[String]]("acronym"))).toList
  }
}

