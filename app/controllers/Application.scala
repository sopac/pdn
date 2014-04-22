package controllers

import play.api._
import play.api.mvc._
import java.util.Date
import models._
import java.io._
import java.nio.file.{ Files, Paths }

object Application extends Controller {

  val STATIC_URL: String = "http://pdn.sop.spc.lab/static/" //Configure Using Apache2 Reverse Proxy

  def photo(id: Int, model: String) = Action {
    var bytes: Array[Byte] = null
    if (model == "wiki") bytes = Wiki.showWiki(id).image.get
    if (model == "document") bytes = DocumentThumbnail.getThumbnail(id).thumbnail.get
    Ok(bytes).as("image/jpg")
  }

  def download(file: String) = Action {
    //val source = scala.io.Source.fromFile("/opt/pdn/doc/" + file)
    //val bytes: Array[Byte] = source.map(_.toByte).toArray
    //source.close()
    val bytes = Files.readAllBytes(Paths.get("/opt/pdn/doc/" + file))
    Ok(bytes).as("application/pdf")
  }

  def index = Action {
    Ok(views.html.index(Alert.recentAlerts(4), Wiki.showFocus, Calendar.recentCalendar(2), Calendar.upcomingCalendar(3), LatestDocument.latestDocuments(4), Wiki.listNews(5), Wiki.listInitiatives))
  }

  def showWiki(id: Int) = Action {
    Ok(views.html.wiki(Wiki.showWiki(id)))
  }

  def listAlert(page: Int, num: Int, sort: String, sortType: String, filter: String) = Action {
    val list = Alert.listAlerts(page, num, sort, sortType, filter)
    val total = Alert.totalAlerts
    Ok(views.html.alerts(list, page, total, filter, Alert.getFilterList("alertsource.name"), Alert.getFilterList("alert.daterecieved"), Alert.getFilterList("alert.countries")))
  }

  def listDocument(page: Int, num: Int, sort: String, sortType: String, filter: String) = Action {
    val list = Document.listDocuments(page, num, sort, sortType, filter)
    val total = Document.totalDocuments
    Ok(views.html.documents(list, page, total, sort, sortType, filter, Document.getFilterList("publicationyear"), Document.getFilterList("materialtype"), Document.getFilterList("language")))
  }

  def showAlert(id: Int) = Action {
    Ok(views.html.alert(Alert.showAlert(id)))
  }

  def showDocument(id: Int) = Action {
    Ok(views.html.document(Document.showDocument(id), DocumentMisc.showDocument(id), DocumentAuthors.listDocumentAuthor(id), DocumentMaterialType.showDocument(id), DocumentUser.showDocument(id)))
  }

  def showDocumentText(id: Int) = Action {
    Ok(views.html.documentText(DocumentText.showDocument(id)))
  }

  def showDocumentHtml(id: Int) = Action {
    val title: String = Document.showDocument(id).title.get
    Ok(views.html.documentHtml(id, title))
  }

  def showDocumentViewer(id: Int) = Action {
    val title: String = Document.showDocument(id).title.get
    val file: String = Document.showDocument(id).filename.get
    Ok(views.html.documentView(id, title, file))
  }

  def testDocument = Action {
    Ok(views.html.testdoc(LatestDocument.latestDocuments(10)))
  }

}