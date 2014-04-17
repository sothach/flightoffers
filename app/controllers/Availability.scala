package controllers

import play.api.mvc._
import play.api.libs.json.{Json, Writes}
import model.{FlightProduct, IATAAirport}
import org.joda.time.DateTime
import data.GenerateFlightData
import GenerateFlightData.Flight
import java.io.{ByteArrayInputStream, ByteArrayOutputStream, PrintStream}
import play.api.libs.iteratee.Enumerator
import scala.concurrent.ExecutionContext
import ExecutionContext.Implicits.global
import play.api.Play._

object Availability extends Controller {

  implicit val dateTimeFormat = new Writes[DateTime] {
    def writes(date: DateTime) = Json.obj(
      "date" -> date.toString("dd-MM-yyyy"),
      "time" -> date.toString("HH:mm:ss"),
      "zone" -> date.getZone.getID)
  }
  implicit val flightFormat = Json.format[Flight]
  implicit val IATAAirportFormat = Json.format[IATAAirport]
  implicit val flightProductFormat = Json.format[FlightProduct]

  def flights(airline: String, date: String, nbRecords: Int) = Action { request =>
    Security.authorized(request) match {
      case Some(true) => getFlights(airline, date, nbRecords)
      case Some(false) | None => Unauthorized
    }
  }

  def getFlights ( airline: String, date: String, nbRecords: Int) = {
    val availDate = parseDate(date)
    SimpleResult(
      body = getFlightsForAirline(airline, availDate, nbRecords),
      header = ResponseHeader(200, contentDisposition(airline, availDate)))
  }

  def getFlightsForAirline ( airline: String, date: DateTime, nbRecords: Int) = {
    val flights = GenerateFlightData.generateFlights(airline, date, nbRecords)
    val output = new ByteArrayOutputStream
    val format = airline match {
      case "EI" => "JSON"
      case "UA" => "XML"
      case "BA" => "CARD"
      case "TK" => "CSV"
      case _ => "XML"
    }
    GenerateFlightData.printFlights(new PrintStream(output), format, flights)
    Enumerator.fromStream(new ByteArrayInputStream(output.toByteArray))
  }

  val dateRX = "(\\d{4})[\\.-]?(\\d{2})[\\.-]?(\\d{2})".r
  def parseDate(date: String) = date match {
    case dateRX (year, month, day) => DateTime.now.withDate (year.toInt, month.toInt, day.toInt)
    case _ => DateTime.now
  }

  def contentDisposition(airline: String, availDate: DateTime) = airline match {
    case "EI" => Map(CONTENT_DISPOSITION -> "inline", CONTENT_TYPE -> "application/json")
    case "UA" => Map(CONTENT_DISPOSITION -> "inline", CONTENT_TYPE -> "text/xml")
    case "BA" => Map(CONTENT_DISPOSITION -> f"attachment; filename=$airline-$availDate.cbl")
    case "TK" => Map(CONTENT_DISPOSITION -> f"attachment; filename=$airline-$availDate.csv")
  }

}
