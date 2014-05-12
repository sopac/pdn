package models

import java.util.{ Date }
import play.api.db._
import play.api.Play.current
import anorm._
import anorm.SqlParser._
import scala.language.postfixOps
import java.text.SimpleDateFormat
import scala.collection.mutable.ListBuffer

case class Contact(id: Pk[Int], organisation_id: Option[Int], designation: Option[String], languages: Option[String], department: Option[String], photo: Option[Array[Byte]], name: Option[String], logo: Option[Array[Byte]], lastname: Option[String], gender: Option[Int], country_iso: Option[String], contacttype: Option[Int], firstname: Option[String], country_name: Option[String], acronym: Option[String])

case class ContactDetail(id: Pk[Int], url1: Option[String], city: Option[String], email2: Option[String], physicaladdress: Option[String], voiceoverip: Option[String], telephone2: Option[String], email1: Option[String], url2: Option[String], satellitephone: Option[String], postaladdress: Option[String], fax: Option[String], postcode: Option[String], telephone1: Option[String], satellitephonesystem: Option[Int], tags: Option[String])

object Contact {

  implicit def rowToByteArray: Column[Array[Byte]] = {
    Column.nonNull[Array[Byte]] { (value, meta) =>
      val MetaDataItem(qualified, nullable, clazz) = meta
      value match {
        case bytes: Array[Byte] => Right(bytes)
        case _ => Left(TypeDoesNotMatch("..."))
      }
    }
  }

  def getFilterList(col: String): List[String] = DB.withConnection {
    implicit connection =>
      var resList = new ListBuffer[String]()
      val sql: SqlQuery = SQL("SELECT DISTINCT " + col + " FROM public.contact AS contact LEFT OUTER JOIN public.organisations AS organisations ON contact.organisation_id = organisations.id LEFT OUTER JOIN public.country AS country ON contact.country_id = country.id ORDER BY " + col)
      if (!col.equals("contact.contacttype")) {
        //resList = sql.as(str(col) *).to[ListBuffer]
        sql().toList.map { r =>
          r.data.map { d =>
            if (d != null) resList += d.toString
          }
        }
      } else {
        resList += "ACADEMIC"
        resList += "CONSULTANT"
        resList += "CROP_AGENCY"
        resList += "DONOR"
        resList += "GOVERNMENT"
        resList += "NGO"
        resList += "REDCROSS"
        resList += "UNITEDNATION"
        resList += "STUDENT"
      }

      //remove duplicates
      resList = resList.distinct
      //sort
      resList = resList.sorted
      return resList.toList
  }

  def totalContacts(): Int = DB.withConnection {
    implicit connection =>
      val count: Long = SQL("select count(*) from contact").as(scalar[Long].single)
      return count.toInt
  }

  def showContact(id: Int): Contact = DB.withConnection {
    implicit connection =>
      val sql: SqlQuery = SQL("SELECT contact.id AS id, organisations.id AS organisation_id, organisations.name, organisations.acronym, organisations.logo, country.name AS country_name, country.iso AS country_iso, contact.firstname, contact.lastname, contact.designation, contact.department, contact.photo, contact.contacttype, contact.gender, contact.languages FROM public.contact AS contact LEFT OUTER JOIN public.organisations AS organisations ON contact.organisation_id = organisations.id LEFT OUTER JOIN public.country AS country ON contact.country_id = country.id WHERE contact.id=" + id)
      sql().map(row =>
        Contact(row[Pk[Int]]("id"), row[Option[Int]]("organisation_id"), row[Option[String]]("designation"), row[Option[String]]("languages"), row[Option[String]]("department"), row[Option[Array[Byte]]]("photo"), row[Option[String]]("name"), row[Option[Array[Byte]]]("logo"), row[Option[String]]("lastname"), row[Option[Int]]("gender"), row[Option[String]]("country_iso"), row[Option[Int]]("contacttype"), row[Option[String]]("firstname"), row[Option[String]]("country_name"), row[Option[String]]("acronym"))).toList.head
  }

  def listContact(page: Int, num: Int, sort: String, sortType: String, filter: String): List[Contact] = DB.withConnection {
    var str: String = "SELECT contact.id AS id, organisations.id AS organisation_id, organisations.name, organisations.acronym, organisations.logo, country.name AS country_name, country.iso AS country_iso, contact.firstname, contact.lastname, contact.designation, contact.department, contact.photo, contact.contacttype, contact.gender, contact.languages FROM public.contact AS contact LEFT OUTER JOIN public.organisations AS organisations ON contact.organisation_id = organisations.id LEFT OUTER JOIN public.country AS country ON contact.country_id = country.id ORDER BY " + sort + " " + sortType + " LIMIT " + num + " OFFSET " + (page * num).toString()

    //filter
    if (!filter.equals("")) {
      val arr = filter.split(":")
      val col = arr(0)
      val value = arr(1)
      str = "SELECT contact.id AS id, organisations.id AS organisation_id, organisations.name, organisations.acronym, organisations.logo, country.name AS country_name, country.iso AS country_iso, contact.firstname, contact.lastname, contact.designation, contact.department, contact.photo, contact.contacttype, contact.gender, contact.languages FROM public.contact AS contact LEFT OUTER JOIN public.organisations AS organisations ON contact.organisation_id = organisations.id LEFT OUTER JOIN public.country AS country ON contact.country_id = country.id WHERE " + col + "='" + value + "'"
      //println(str)
      if (col.equals("contact.contacttype")) {
        val x = value match {
          case "ACADEMIC" => 0
          case "CONSULTANT" => 1
          case "CROP_AGENCY" => 2
          case "DONOR" => 3
          case "GOVERNMENT" => 4
          case "NGO" => 5
          case "REDCROSS" => 6
          case "UNITEDNATION" => 7
          case "STUDENT" => 8
        }
        str = "SELECT contact.id AS id, organisations.id AS organisation_id, organisations.name, organisations.acronym, organisations.logo, country.name AS country_name, country.iso AS country_iso, contact.firstname, contact.lastname, contact.designation, contact.department, contact.photo, contact.contacttype, contact.gender, contact.languages FROM public.contact AS contact LEFT OUTER JOIN public.organisations AS organisations ON contact.organisation_id = organisations.id LEFT OUTER JOIN public.country AS country ON contact.country_id = country.id WHERE " + col + "=" + x + ""
      }
    }

    implicit connection =>
      val sql: SqlQuery = SQL(str)
      sql().map(row =>
        Contact(row[Pk[Int]]("id"), row[Option[Int]]("organisation_id"), row[Option[String]]("designation"), row[Option[String]]("languages"), row[Option[String]]("department"), row[Option[Array[Byte]]]("photo"), row[Option[String]]("name"), row[Option[Array[Byte]]]("logo"), row[Option[String]]("lastname"), row[Option[Int]]("gender"), row[Option[String]]("country_iso"), row[Option[Int]]("contacttype"), row[Option[String]]("firstname"), row[Option[String]]("country_name"), row[Option[String]]("acronym"))).toList
  }
}

object ContactDetail {
  def showContact(id: Int): ContactDetail = DB.withConnection {
    implicit connection =>
      val sql: SqlQuery = SQL("SELECT id, city, email1, email2, fax, physicaladdress, postcode, postaladdress, satellitephone, satellitephonesystem, tags, telephone1, telephone2, url1, url2, voiceoverip FROM public.contact AS contact where id=" + id)
      sql().map(row =>
        ContactDetail(row[Pk[Int]]("id"), row[Option[String]]("url1"), row[Option[String]]("city"), row[Option[String]]("email2"), row[Option[String]]("physicaladdress"), row[Option[String]]("voiceoverip"), row[Option[String]]("telephone2"), row[Option[String]]("email1"), row[Option[String]]("url2"), row[Option[String]]("satellitephone"), row[Option[String]]("postaladdress"), row[Option[String]]("fax"), row[Option[String]]("postcode"), row[Option[String]]("telephone1"), row[Option[Int]]("satellitephonesystem"), row[Option[String]]("tags"))).toList.head
  }
}

