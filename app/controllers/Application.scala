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
    if (model == "media") bytes = Media.showMedia(id).thumbnail.get
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
    Ok(views.html.index(Alert.recentAlerts(4), Wiki.showFocus, Calendar.recentCalendar(2), Calendar.upcomingCalendar(3), LatestDocument.latestDocuments(3), Wiki.listNews(5), Wiki.listInitiatives))
  }

  def showWiki(id: Int) = Action {
    Ok(views.html.wiki(Wiki.showWiki(id)))
  }

  def listAlert(page: Int, num: Int, sort: String, sortType: String, filter: String) = Action {
    val list = Alert.listAlerts(page, num, sort, sortType, filter)
    val total = Alert.totalAlerts
    Ok(views.html.alerts(list, page, total, filter, Alert.getFilterList("alertsource.name"), Alert.getFilterList("alert.daterecieved"), Alert.getFilterList("alert.countries")))
  }

  def listCalendar(page: Int, num: Int, sort: String, sortType: String, filter: String) = Action {
    val list = Calendar.listCalendar(page, num, sort, sortType, filter)
    val total = Calendar.totalCalendars
    Ok(views.html.calendars(list, page, total, sort, sortType, filter, Calendar.getFilterList("eventtype"), Calendar.getFilterList("calendaryear"), Calendar.getFilterList("country.name")))

  }

  def listContact(page: Int, num: Int, sort: String, sortType: String, filter: String) = Action {
    val list = Contact.listContact(page, num, sort, sortType, filter)
    val total = Contact.totalContacts
    Ok(views.html.contacts(list, page, total, filter, Contact.getFilterList("organisations.name"), Contact.getFilterList("country.name"), Contact.getFilterList("contact.contacttype"))) // Alert.getFilterList("alertsource.name"), Alert.getFilterList("alert.daterecieved"), Alert.getFilterList("alert.countries")))
  }

  def listDocument(page: Int, num: Int, sort: String, sortType: String, filter: String) = Action {
    val list = Document.listDocuments(page, num, sort, sortType, filter)
    val total = Document.totalDocuments
    Ok(views.html.documents(list, page, total, sort, sortType, filter, Document.getFilterList("publicationyear"), Document.getFilterList("materialtype"), Document.getFilterList("language")))
  }

  def listMedia(page: Int, num: Int, sort: String, sortType: String, filter: String) = Action {
    val list = Media.listMedia(page, num, sort, sortType, filter)
    val total = Media.totalMedia
    val filterType = Media.mediaType
    Ok(views.html.medias(list, page, total, sort, sortType, filter, filterType))
  }

  def showAlert(id: Int) = Action {
    Ok(views.html.alert(Alert.showAlert(id)))
  }

  def showContact(id: Int) = Action {
    Ok(views.html.contact(Contact.showContact(id), ContactDetail.showContact(id)))
  }

  def showMedia(id: Int) = Action {
    Ok(views.html.media(Media.showMedia(id)))
  }

  def showCalendar(id: Int) = Action {
    Ok(views.html.calendar(Calendar.showCalendar(id), CalendarLink.getCalendarLinks(id), CalendarOrganisation.getCalendarOrganisations(id)))
  }

  def showDocument(id: Int) = Action {
    Ok(views.html.document(Document.showDocument(id), DocumentMisc.showDocument(id), DocumentAuthors.listDocumentAuthor(id), DocumentMaterialType.showDocument(id), DocumentUser.showDocument(id), DocumentRelation.get(id, "Topic"), DocumentRelation.get(id, "Country"), DocumentRelation.get(id, "Organisation"), DocumentRelation.get(id, "Meeting")))
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