package models

import java.util.{ Date }
import play.api.db._
import play.api.Play.current
import anorm._
import anorm.SqlParser._
import scala.language.postfixOps
import scala.collection.mutable.ListBuffer

case class LatestDocument(id: Pk[Int], uploaddate: Option[Date], description: Option[String], title: Option[String], filename: Option[String])

case class Document(id: Pk[Int], catalougingsource: Option[String], varianttitle: Option[String], publicationmonth: Option[Int], generalnote: Option[String], relatednames: Option[String], uploaddate: Option[Date], sourceofacquisition: Option[String], relatedcorporations: Option[String], placeofpublication: Option[String], geographicalareacode: Option[String], publicationyear: Option[Int], publisher: Option[String], description: Option[String], filename: Option[String], filesize: Option[Long], title: Option[String], corporateauthor: Option[String], publicationday: Option[Int], issn: Option[String], series: Option[String], isbn: Option[String])

case class DocumentThumbnail(id: Pk[Int], thumbnail: Option[Array[Byte]])

case class DocumentMisc(id: Pk[Int], edition: Option[String], language_id: Option[Int], targetaudience: Option[String], legacyid: Option[Int], featured: Option[Boolean], disaster: Option[String], bibliographynote: Option[String], tags: Option[String])

case class DocumentAuthors(id: Pk[Int], tile: Option[Int], designation: Option[String], document_id: Option[Int], organisation: Option[String], name: Option[String], email: Option[String])

case class DocumentMaterialType(id: Pk[Int], materialType: Option[String])

case class DocumentUser(id: Pk[Int], designation: Option[String], photo: Option[Array[Byte]], organisation: Option[String], name: Option[String], email: Option[String])

case class DocumentText(id: Pk[Int], title: Option[String], text: Option[String])

object LatestDocument {
  def latestDocuments(num: Int): List[LatestDocument] = DB.withConnection {
    implicit connection =>
      val sql: SqlQuery = SQL("select id, title, description, uploaddate, filename from document order by id desc limit " + num)
      sql().map(row =>
        LatestDocument(row[Pk[Int]]("id"), row[Option[Date]]("uploaddate"), row[Option[String]]("description"), row[Option[String]]("title"), row[Option[String]]("filename"))).toList
  }
}

object Document {

  def showDocument(id: Int): Document = DB.withConnection {
    implicit connection =>
      val sql: SqlQuery = SQL("select id, title, varianttitle, description, corporateauthor, relatednames, relatedcorporations, uploaddate, publicationDay, publicationMonth, publicationYear, publisher, placeofPublication, filename, filesize, catalougingsource, sourceofacquisition, geographicalareacode, generalnote, series, issn, isbn FROM document WHERE id=" + id)
      sql().map(row =>
        Document(row[Pk[Int]]("id"), row[Option[String]]("catalougingsource"), row[Option[String]]("varianttitle"), row[Option[Int]]("publicationmonth"), row[Option[String]]("generalnote"), row[Option[String]]("relatednames"), row[Option[Date]]("uploaddate"), row[Option[String]]("sourceofacquisition"), row[Option[String]]("relatedcorporations"), row[Option[String]]("placeofpublication"), row[Option[String]]("geographicalareacode"), row[Option[Int]]("publicationyear"), row[Option[String]]("publisher"), row[Option[String]]("description"), row[Option[String]]("filename"), row[Option[Long]]("filesize"), row[Option[String]]("title"), row[Option[String]]("corporateauthor"), row[Option[Int]]("publicationday"), row[Option[String]]("issn"), row[Option[String]]("series"), row[Option[String]]("isbn"))).toList.head
  }

  def listDocuments(page: Int, num: Int, sort: String, sortType: String, filter: String): List[Document] = DB.withConnection {
    implicit connection =>
      var str: String = "select id, title, varianttitle, description, corporateauthor, relatednames, relatedcorporations, uploaddate, publicationDay, publicationMonth, publicationYear, publisher, placeofPublication, filename, filesize, catalougingsource, sourceofacquisition, geographicalareacode, generalnote, series, issn, isbn FROM document ORDER BY " + sort + " " + sortType + " LIMIT " + num + " OFFSET " + (page * num).toString()

      //filter
      if (!filter.equals("")) {
        val arr = filter.split(":")
        val col = arr(0).trim()
        val value = arr(1).trim()

        if (col.equals("language")) {
          if (value.equals("English"))
            str = "select id, title, varianttitle, description, corporateauthor, relatednames, relatedcorporations, uploaddate, publicationDay, publicationMonth, publicationYear, publisher, placeofPublication, filename, filesize, catalougingsource, sourceofacquisition, geographicalareacode, generalnote, series, issn, isbn FROM document where language_id=1 ORDER BY " + sort + " " + sortType + " LIMIT " + num + " OFFSET " + (page * num).toString()
          if (value.equals("French"))
            str = "select id, title, varianttitle, description, corporateauthor, relatednames, relatedcorporations, uploaddate, publicationDay, publicationMonth, publicationYear, publisher, placeofPublication, filename, filesize, catalougingsource, sourceofacquisition, geographicalareacode, generalnote, series, issn, isbn FROM document where language_id=2 ORDER BY " + sort + " " + sortType + " LIMIT " + num + " OFFSET " + (page * num).toString()
        }

        if (col.equals("materialtype")) {
          //select id from documentmaterialtypes where type=value
          val material_id = SQL("select id from documentmaterialtypes where type='" + value + "'").as(scalar[Int].single)
          str = "select id, title, varianttitle, description, corporateauthor, relatednames, relatedcorporations, uploaddate, publicationDay, publicationMonth, publicationYear, publisher, placeofPublication, filename, filesize, catalougingsource, sourceofacquisition, geographicalareacode, generalnote, series, issn, isbn FROM document where materialtype_id=" + material_id + " ORDER BY " + sort + " " + sortType + " LIMIT " + num + " OFFSET " + (page * num).toString()
        }

        if (col.equals("publicationyear")) {
          str = "select id, title, varianttitle, description, corporateauthor, relatednames, relatedcorporations, uploaddate, publicationDay, publicationMonth, publicationYear, publisher, placeofPublication, filename, filesize, catalougingsource, sourceofacquisition, geographicalareacode, generalnote, series, issn, isbn FROM document where publicationYear=" + value + " ORDER BY " + sort + " " + sortType + " LIMIT " + num + " OFFSET " + (page * num).toString()
        }

      }

      val sql: SqlQuery = SQL(str)
      sql().map(row =>
        Document(row[Pk[Int]]("id"), row[Option[String]]("catalougingsource"), row[Option[String]]("varianttitle"), row[Option[Int]]("publicationmonth"), row[Option[String]]("generalnote"), row[Option[String]]("relatednames"), row[Option[Date]]("uploaddate"), row[Option[String]]("sourceofacquisition"), row[Option[String]]("relatedcorporations"), row[Option[String]]("placeofpublication"), row[Option[String]]("geographicalareacode"), row[Option[Int]]("publicationyear"), row[Option[String]]("publisher"), row[Option[String]]("description"), row[Option[String]]("filename"), row[Option[Long]]("filesize"), row[Option[String]]("title"), row[Option[String]]("corporateauthor"), row[Option[Int]]("publicationday"), row[Option[String]]("issn"), row[Option[String]]("series"), row[Option[String]]("isbn"))).toList
  }

  def totalDocuments(): Int = DB.withConnection {
    implicit connection =>
      val count: Long = SQL("select count(*) from document").as(scalar[Long].single)
      return count.toInt
  }

  def getFilterList(col: String): List[String] = DB.withConnection {
    implicit connection =>

      var resList = new ListBuffer[String]()

      if (col.equals("materialtype")) {
        val sql: SqlQuery = SQL("SELECT distinct type FROM documentmaterialtypes ORDER BY type")
        resList = sql.as(str("documentmaterialtypes.type") *).to[ListBuffer]
      } else if (col.equals("language")) {
        resList += "English"
        resList += "French"
      } else {
        val sql: SqlQuery = SQL("SELECT distinct " + col + " FROM document ORDER BY " + col + " desc")
        val tmp: List[Int] = sql.as(int(col) *)
        for (t <- tmp) resList += t.toString
      }
      //remove duplicates
      resList -= "0"
      resList = resList.distinct
      //sort
      resList = resList.sorted
      return resList.toList
  }

}

object DocumentThumbnail {
  implicit def rowToByteArray: Column[Array[Byte]] = {
    Column.nonNull[Array[Byte]] { (value, meta) =>
      val MetaDataItem(qualified, nullable, clazz) = meta
      value match {
        case bytes: Array[Byte] => Right(bytes)
        case _ => Left(TypeDoesNotMatch("..."))
      }
    }
  }
  def getThumbnail(id: Int): DocumentThumbnail = DB.withConnection {
    implicit connection =>
      val sql: SqlQuery = SQL("select id, thumbnail from document where id =" + id)
      sql().map(row =>
        DocumentThumbnail(row[Pk[Int]]("id"), row[Option[Array[Byte]]]("thumbnail"))).toList.head
  }
}

object DocumentMisc { //id, bibliographynote, disaster, edition, legacyid, tags, featured, targetaudience, language_id
  def showDocument(id: Int): DocumentMisc = DB.withConnection {
    implicit connection =>
      val sql: SqlQuery = SQL("select id, bibliographynote, disaster, edition, legacyid, tags, featured, targetaudience, language_id from document order by id")
      sql().map(row =>
        DocumentMisc(row[Pk[Int]]("id"), row[Option[String]]("edition"), row[Option[Int]]("language_id"), row[Option[String]]("targetaudience"), row[Option[Int]]("legacyid"), row[Option[Boolean]]("featured"), row[Option[String]]("disaster"), row[Option[String]]("bibliographynote"), row[Option[String]]("tags"))).toList.head
  }
}

object DocumentAuthors {
  def listDocumentAuthor(id: Int): List[DocumentAuthors] = DB.withConnection {
    implicit connection =>
      val sql: SqlQuery = SQL("SELECT document.id, documentauthor.* FROM public.documentauthor AS documentauthor INNER JOIN public.document AS document ON documentauthor.document_id = document.id WHERE document.id=" + id)
      sql().map(row =>
        DocumentAuthors(row[Pk[Int]]("id"), row[Option[Int]]("tile"), row[Option[String]]("designation"), row[Option[Int]]("document_id"), row[Option[String]]("organisation"), row[Option[String]]("name"), row[Option[String]]("email"))).toList
  }
}

object DocumentMaterialType {
  def showDocument(id: Int): DocumentMaterialType = DB.withConnection {
    implicit connection =>
      val sql: SqlQuery = SQL("SELECT document.id, documentmaterialtypes.type FROM public.document AS document, public.documentmaterialtypes AS documentmaterialtypes WHERE document.materialtype_id = documentmaterialtypes.id AND document.id=" + id)
      sql().map(row =>
        DocumentMaterialType(row[Pk[Int]]("id"), row[Option[String]]("type"))).toList.head
  }
}

object DocumentUser {
  implicit def rowToByteArray: Column[Array[Byte]] = {
    Column.nonNull[Array[Byte]] { (value, meta) =>
      val MetaDataItem(qualified, nullable, clazz) = meta
      value match {
        case bytes: Array[Byte] => Right(bytes)
        case _ => Left(TypeDoesNotMatch("..."))
      }
    }
  }
  def showDocument(id: Int): DocumentUser = DB.withConnection {
    implicit connection =>
      val sql: SqlQuery = SQL("SELECT document.id, users.name, users.email, users.organisation, users.designation, users.photo FROM public.document AS document LEFT OUTER JOIN public.users AS users ON document.user_id = users.id WHERE document.id=" + id)
      sql().map(row =>
        DocumentUser(row[Pk[Int]]("id"), row[Option[String]]("designation"), row[Option[Array[Byte]]]("photo"), row[Option[String]]("organisation"), row[Option[String]]("name"), row[Option[String]]("email"))).toList.head
  }
}

object DocumentLinks {
}

object DocumentText {
  def showDocument(id: Int): DocumentText = DB.withConnection {
    implicit connection =>
      val sql: SqlQuery = SQL("SELECT document.id, document.title, documenttext.text FROM public.document AS document, public.documenttext AS documenttext WHERE document.id = documenttext.documentid AND document.id=" + id)
      sql().map(row =>
        DocumentText(row[Pk[Int]]("id"), row[Option[String]]("title"), row[Option[String]]("text"))).toList.head
  }
}

object DocumentRelations {
}

