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
import scala.concurrent._
import scala.concurrent.duration._
import java.net.URLEncoder

object Search extends Controller {

  implicit val context = scala.concurrent.ExecutionContext.Implicits.global

  val SOLR_SERVER: String = Application.STATIC_URL.replace("/static/", ":8888/solr/pdn/lucid?")
  val URL_APPEND: String = "&wt=json&indent=true&start=0&rows=10"

  def searchSolr(query: String, source: String): ListBuffer[Tuple3[String, String, String]] = {
    var resList = new ListBuffer[Tuple3[String, String, String]]()
    val url = SOLR_SERVER + "q=" + URLEncoder.encode(query, "UTF-8") + "&fq=data_source_name:" + source + URL_APPEND
    val holder: WSRequestHolder = WS.url(url)
    val futureResponse: Future[Response] = holder.get()
    val response = Await.result(futureResponse, 10 seconds)
    val res = (response.json \ "response" \ "docs").as[List[JsObject]]
    res.map { r =>
      if (source.equals("documents")) {
        val id = (r \ "id")
        val title = (r \ "title")(1).toString + ", " + (r \ "title")(0).toString()
        var description = (r \ "description").toString() //+ " | " + (r \ "author")(0)
        val author = (r \ "author")(0).toString()
        if (description.trim().equals("")) description = "None"
        if (!author.startsWith("JsUndefined")) description += " | " + author
        val tuple = (id.toString().replaceAll("\"", "").trim(), title.toString().replaceAll("\"", "").trim(), description.toString().replaceAll("\"", "").trim())
        resList += tuple
      }
      if (source.equals("contacts")) {
        val id = (r \ "id").toString()
        val title = (r \ "firstname") + " " + (r \ "lastname")
        val description = (r \ "designation") + ", " + (r \ "organisation") + ", " + (r \ "country")
        val tuple = (id.toString().replaceAll("\"", "").trim(), title.toString().replaceAll("\"", "").trim(), description.toString().replaceAll("\"", "").trim())
        resList += tuple
      }
      if (source.equals("alerts")) {
        val id = (r \ "id").toString()
        val title = (r \ "subject")
        var content = (r \ "content").toString().replaceAll("\n\n", " ")
        if (content.startsWith("<")) content = "HTML Content"
        if (content.length() > 50) content = content.substring(0, 50) + " ..."
        val description = content + "<br/>" + (r \ "daterecieved").toString().substring(0, 11)
        val tuple = (id.toString().replaceAll("\"", "").trim(), title.toString().replaceAll("\"", "").trim(), description.toString().replaceAll("\"", "").trim())
        resList += tuple
      }
      if (source.equals("calendar")) {
        val id = (r \ "id").toString()
        val title = (r \ "title")(0)
        val description = (r \ "venue") + " " + (r \ "name")
        val tuple = (id.toString().replaceAll("\"", "").trim(), title.toString().replaceAll("\"", "").trim(), description.toString().replaceAll("\"", "").trim())
        resList += tuple
      }
      if (source.equals("media")) {
        val id = (r \ "id").toString()
        val title = (r \ "title")(0)
        val description = (r \ "description")
        val tuple = (id.toString().replaceAll("\"", "").trim(), title.toString().replaceAll("\"", "").trim(), description.toString().replaceAll("\"", "").trim())
        resList += tuple
      }
      if (source.equals("files")) {
        val id = "1000" //(r \ "id").toString()
        val title = (r \ "title")(0)
        val description = (r \ "author")(0)
        val tuple = (id.toString().replaceAll("\"", "").trim(), title.toString().replaceAll("\"", "").trim(), description.toString().replaceAll("\"", "").trim())
        resList += tuple
      }
      if (source.equals("wiki")) {
        val id = (r \ "id").toString()
        val title = (r \ "title")(0)
        val description = (r \ "title")(0)
        val tuple = (id.toString().replaceAll("\"", "").trim(), title.toString().replaceAll("\"", "").trim(), description.toString().replaceAll("\"", "").trim())
        resList += tuple
      }
    }
    return resList
  }

  def search(query: String, sources: String) = Action { //source = all(default), alerts, contacts, documents, media, files, calendar

    //var alerts = new ListBuffer[Tuple3[String, String, String]]() //tuple - id, title, description, score

    val documents = searchSolr(query, "documents").toList
    val contacts = searchSolr(query, "contacts").toList
    val alerts = searchSolr(query, "alerts").toList
    val calendar = searchSolr(query, "calendar").toList
    val media = searchSolr(query, "media").toList
    val files = searchSolr(query, "files").toList
    val wiki = searchSolr(query, "wiki").toList

    val total: Int = contacts.size + documents.size + alerts.size + calendar.size + media.size + files.size + wiki.size

    Ok(views.html.search(query, total, sources, documents, contacts, alerts, calendar, media, files, wiki))
  }
}