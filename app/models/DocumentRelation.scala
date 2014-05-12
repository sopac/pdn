package models

import play.api.db._
import play.api.Play.current
import anorm._
import anorm.SqlParser._
import scala.language.postfixOps
import java.text.SimpleDateFormat
import scala.collection.mutable.ListBuffer

case class DocumentRelation(id: Pk[Int], folder: Option[String], description: Option[String], document_id: Option[Int], treeorder: Option[Int])

object DocumentRelation {

  def listDocumentRelation(id: Int): List[DocumentRelation] = DB.withConnection {
    implicit connection =>
      val sql: SqlQuery = SQL("select * from documentrelationitem where document_id=" + id + " order by treeorder")
      sql().map(row =>
        DocumentRelation(row[Pk[Int]]("id"), row[Option[String]]("folder"), row[Option[String]]("description"), row[Option[Int]]("document_id"), row[Option[Int]]("treeorder"))).toList
  }

  def get(id: Int, t: String): List[String] = DB.withConnection { // t = Topic, Country, Organisation, Meeting
    var resList = new ListBuffer[String]()
    val drl = listDocumentRelation(id)
    var valid = false
    drl.map { r =>
      if (r.folder.getOrElse("").trim().equals("") && r.description.get.trim().equals(t)) valid = true
      if (r.folder.getOrElse("").trim().equals("") && !r.description.get.trim().equals(t)) valid = false
      if (valid) resList += r.description.get
    }
    resList -= t
    return resList.toList
  }

}