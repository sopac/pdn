package utils

import java.sql._

object AnormCodeGen extends App {

  try {
    //val sql = "SELECT alert.*, alertsource.alerttype, alertsource.email AS source_email, alertsource.name AS source_name, alertsource.url AS source_url FROM public.alert AS alert, public.alertsource AS alertsource WHERE alert.source_id = alertsource.id"
    //val sql = "select * from wiki order by id desc"
    //val sql = "SELECT calendar.*, country.name AS country_name, country.iso AS country_iso, calendarlink.link, calendarlink.description FROM public.calendar AS calendar LEFT OUTER JOIN public.country AS country ON calendar.country_id = country.id LEFT OUTER JOIN public.calendarlink AS calendarlink ON calendarlink.calendar_id = calendar.id"
    //val sql = "select * from media order by id desc"
    //val sql = "SELECT document.*, documentmaterialtypes.type AS material_type, documentlink.link, documentauthor.name AS author_name, documentauthor.id AS author_id FROM public.document AS document LEFT OUTER JOIN public.documentmaterialtypes AS documentmaterialtypes ON document.materialtype_id = documentmaterialtypes.id LEFT OUTER JOIN public.documentauthor AS documentauthor ON document.id = documentauthor.document_id LEFT OUTER JOIN public.documentlink AS documentlink ON document.id = documentlink.document_id "
    //val sql = "select id, title, description, uploaddate from document order by id desc"
    //val sql = "select id, title, varianttitle, description, corporateauthor, relatednames, relatedcorporations, uploaddate, publicationDay, publicationMonth, publicationYear, publisher, placeofPublication, filename, filesize, catalougingsource, sourceofacquisition, geographicalareacode, generalnote, series, issn, isbn FROM document order by id;"
    //val sql = "select id, thumbnail from document order by id"
    //val sql = "select id, bibliographynote, disaster, edition, legacyid, tags, featured, targetaudience, language_id from document order by id"
    //val sql = "SELECT document.id, documentauthor.* FROM public.documentauthor AS documentauthor RIGHT OUTER JOIN public.document AS document ON documentauthor.document_id = document.id order by document.id"
    //val sql = "SELECT document.id, documentmaterialtypes.type FROM public.document AS document, public.documentmaterialtypes AS documentmaterialtypes WHERE document.materialtype_id = documentmaterialtypes.id Order By document.id"
    //val sql = "SELECT document.id, users.name, users.email, users.organisation, users.designation, users.photo FROM public.document AS document, public.users AS users WHERE document.user_id = users.id order by users.id"
    //val sql = "SELECT document.id, documenttext.text FROM public.document AS document, public.documenttext AS documenttext WHERE document.id = documenttext.documentid order by document.id"
    //val sql = "SELECT calendar.id, calendarlink.description, calendarlink.link FROM public.calendarlink AS calendarlink RIGHT OUTER JOIN public.calendar AS calendar ON calendarlink.calendar_id = calendar.id order by calendar.id"
    //val sql = "SELECT calendar.id, organisations.name, organisations.acronym, organisations.id as organisation_id FROM public.organisations_calendar AS organisations_calendar RIGHT OUTER JOIN public.calendar AS calendar ON organisations_calendar.calendars_id = calendar.id, public.organisations AS organisations WHERE organisations_calendar.organisation_id = organisations.id order by calendar.id"
    //val sql = "select * from media order by id"
    //val sql = "SELECT contact.*, organisations.id AS organisation_id, organisations.name, organisations.acronym, organisations.logo, country.name AS country_name, country.iso AS country_iso FROM public.contact AS contact LEFT OUTER JOIN public.organisations AS organisations ON contact.organisation_id = organisations.id LEFT OUTER JOIN public.country AS country ON contact.country_id = country.id order by contact.id"
    //val sql = "SELECT organisations.id AS organisation_id, organisations.name, organisations.acronym, organisations.logo, country.name AS country_name, country.iso AS country_iso, contact.firstname, contact.lastname, contact.designation, contact.department, contact.photo, contact.contacttype, contact.gender, contact.languages FROM public.contact AS contact LEFT OUTER JOIN public.organisations AS organisations ON contact.organisation_id = organisations.id LEFT OUTER JOIN public.country AS country ON contact.country_id = country.id"
    //val sql = "SELECT id, city, email1, email2, fax, physicaladdress, postcode, postaladdress, satellitephone, satellitephonesystem, tags, telephone1, telephone2, url1, url2, voiceoverip FROM public.contact AS contact"
    val sql = "select * from documentrelationitem where document_id=98708 order by treeorder"

    Class.forName("org.postgresql.Driver")
    val conn = DriverManager.getConnection("jdbc:postgresql://localhost/pdn_db", "postgres", "erlang44")
    val stmt = conn.createStatement
    val rs = stmt.executeQuery(sql)
    var table = sql.toLowerCase()
    table = table.substring(table.indexOf("from ") + 5, table.length()).trim()
    table = table.substring(0, table.indexOf(" ")).trim()
    table = table.replace("public.", " ").trim()
    table = table.capitalize
    println(table + "\r\n")
    var map = collection.mutable.Map[String, String]()
    for (i <- 1 to rs.getMetaData().getColumnCount()) {
      val n = rs.getMetaData().getColumnName(i)
      var t = rs.getMetaData().getColumnTypeName(i)
      //println(n + " : " + t)
      if (t.startsWith("int")) t = "Int"
      if (t.equals("serial")) t = "Int"
      if (t.equals("varchar")) t = "String"
      if (t.equals("timestamp")) t = "Date"
      if (t.equals("bool")) t = "Boolean"
      if (t.equals("bytea")) t = "Array[Byte]"
      if (t.equals("float8")) t = "Double"
      if (t.equals("int8")) t = "Long"
      if (!n.equals("id")) map += (n -> t)
      //println(n)
    }
    conn.close
    val nl = System.getProperty("line.separator")

    var cc = "case class " + table + "("
    cc += "id: Pk[Int], "
    for ((n, t) <- map) {
      cc += n + ": " + "Option[" + t + "], "
    }
    cc += ")end"
    cc = cc.replace(", )end", ")\r\n")
    println(cc)

    var obj = "object " + table + " {" + nl
    obj += "  def list" + table + ": List[" + table + "] = DB.withConnection {" + nl
    obj += "    implicit connection =>" + nl
    obj += "      val sql: SqlQuery = SQL('" + sql + "')" + nl
    obj += "      sql().map(row =>" + nl
    obj += "        " + table + "(row[Pk[Int]]('id'),"
    for ((n, t) <- map) {
      obj += "row[Option[" + t + "]]('" + n + "'),"
    }
    obj = obj.substring(0, obj.lastIndexOf(",")).trim()
    obj += "    )).toList" + nl
    obj += "   } " + nl + "}"
    obj = obj.replaceAll("'", "\"") + nl
    println(obj)
  } catch {
    case e: Exception => e.printStackTrace()
  }

}