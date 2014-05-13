package controllers

import play.api._
import play.api.mvc._
import java.util.Date
import models._
import java.io._
import play.api.libs.ws._
import scala.concurrent.Future
import play.api.libs.ws.WS._
import scala.collection.mutable.ListBuffer
import play.api.libs.json._

object Search extends Controller {

  implicit val context = scala.concurrent.ExecutionContext.Implicits.global

  val SOLR_SERVER: String = Application.STATIC_URL.replace("/static/", ":8888/solr/pdn/lucid?")
  val URL_APPEND: String = "&wt=json&indent=true"

  def searchContacts(query: String): ListBuffer[Tuple3[String, String, String]] = {
    var contacts = new ListBuffer[Tuple3[String, String, String]]()
    val source = "contacts"
    val url = SOLR_SERVER + "q=" + query + "&fq=data_source_name:" + source + URL_APPEND
    val holder: WSRequestHolder = WS.url(url)
    val futureResponse: Future[Response] = holder.get()
    futureResponse.map { response =>
      val res = (response.json \ "response" \ "docs").as[List[JsObject]]
      res.map { r =>
        val id = (r \ "id").toString()
        val title = (r \ "firstname").toString() + " " + (r \ "lastname").toString()
        val description = (r \ "designation").toString() + " " + (r \ "organisation").toString() + " " + (r \ "country").toString()
        val tuple = (id, title, description)
        contacts += tuple
      }
      return contacts
    }
  }

  def searchDocuments(query: String): ListBuffer[Tuple3[String, String, String]] = {
    var documents = new ListBuffer[Tuple3[String, String, String]]()
    val source = "documents"
    val url = SOLR_SERVER + "q=" + query + "&fq=data_source_name:" + source + URL_APPEND
    val holder: WSRequestHolder = WS.url(url)
    val futureResponse: Future[Response] = holder.get()
    futureResponse.map { response =>
      val res = (response.json \ "response" \ "docs").as[List[JsObject]]
      res.map { r =>
        val id = (r \ "id").toString()
        val title = (r \ "title").toString()
        val description = (r \ "description").toString() + " | " + (r \ "author").toString()
        val tuple = (id, title, description)
        println(title)
        documents += tuple
      }
    }
    return documents
  }

  def search(query: String, sources: String) = Action { //source = all(default), alerts, contacts, documents, media, files, calendar

    var alerts = new ListBuffer[Tuple3[String, String, String]]() //tuple - id, title, description, score
    var contacts = new ListBuffer[Tuple3[String, String, String]]()
    var calendar = new ListBuffer[Tuple3[String, String, String]]()
    var documents = new ListBuffer[Tuple3[String, String, String]]()
    var media = new ListBuffer[Tuple3[String, String, String]]()

    contacts = searchContacts(query)
    documents = searchDocuments(query)

    val total: Int = contacts.size + documents.size + alerts.size + calendar.size + media.size

    Ok(views.html.search(query, total, sources, documents.toList, contacts.toList))
  }
}