package data

import org.joda.money.{Money, CurrencyUnit}
import org.joda.time.{LocalTime, DateTime}
import org.joda.time.format.DateTimeFormat
import java.math.{RoundingMode, BigDecimal}
import scala.util.Random
import scala.annotation.tailrec
import java.io.PrintStream


object GenerateFlightData {

  val Airlines = List("EI", "FR", "TK", "BA", "AF", "LH", "UA")
  val OperatorCurrencies = Map(
    "EI" -> CurrencyUnit.EUR,
    "FR" -> CurrencyUnit.EUR,
    "EI" -> CurrencyUnit.EUR,
    "TK" -> CurrencyUnit.getInstance("TRY"),
    "BA" -> CurrencyUnit.GBP,
    "AF" -> CurrencyUnit.EUR,
    "LH" -> CurrencyUnit.EUR,
    "UA" -> CurrencyUnit.USD)

  def printFlights(out: PrintStream, format: String, flights: IndexedSeq[Flight]) {
    format match {
      case "XML" =>
        out.println ("<Availability>")
      case "JSON" =>
        out.println ("{ \"availability\": [")
      case _ =>
    }
    var counter = 0
    for (flight <- flights.sorted)
    {
      format match {
        case "CSV" =>
          out.println(flight.toCSV)
        case "CARD" =>
          out.println (flight.toCardFormat)
        case "XML" =>
          out.println (flight.toXml.toString())
        case "JSON" =>
          if (counter > 0) {
            out.println(",")
          }
          out.print(flight.toJson)
        case _ =>
          out.println (flight.toString)
      }
      counter = counter + 1
    }
    format match {
      case "XML" =>
        out.println ("</Availability>")
      case "JSON" =>
        out.println ("\n]}")
      case _ =>
    }
  }

  def Airports = Array("DUB", "LHR", "MUC", "IST", "JFK", "BOS")

  def randomInt(min: Int, max: Int): Int = Random.nextInt(max-min+1)+min

  @tailrec def randomIntExcept(min: Int, max: Int, not: Int): Int = {
    val result = randomInt(min,max)
    if (result != not) {
      result
    } else {
      randomIntExcept(min,max,not)
    }
  }

  def generateFlights(operator: String, departureDate: DateTime, count: Int) =
    for { _ <- 0 to count
      departAirport = randomInt(0, Airports.length - 1)
      arriveAirport = randomIntExcept(0, Airports.length - 1, departAirport)
      flightTime = randomInt(90, 390)
      departs = departureDate.withTime(randomInt(0, 11), randomInt(0, 59), 0, 0)
    } yield Flight(operator, f"$operator${randomInt(10, 600)}", departs,
              departs.plusMinutes(flightTime), flightTime,
              Airports(departAirport), Airports(arriveAirport))

  case class Flight(operator: String,
                    flightNumber: String,
                    departureDate: DateTime,
                    arrivalDate: DateTime,
                    flightTime: Int,
                    origin: String,
                    destination: String) extends Ordered[Flight] {

    val basePrice = flightTime / 4 * randomInt(1, 4)
    val currency = OperatorCurrencies(operator)
    val fares = Map(
      "FIF.1" -> Money.of(currency, basePrice * 4),
      "FIF.2" -> Money.of(currency, basePrice / 4),
      "FIF.3" -> Money.of(currency, (basePrice * 4) / 5),
      "CIF.1" -> Money.of(currency, basePrice * 2),
      "CIF.2" -> Money.of(currency, basePrice / 4),
      "CIF.3" -> Money.of(currency, (basePrice * 2) / 5),
      "YIF.1" -> Money.of(currency, basePrice),
      "YIF.2" -> Money.of(currency, basePrice / 4),
      "YIF.3" -> Money.of(currency, basePrice / 5))

    def toJson = {
      val hours = flightTime / 60
      val mins = flightTime.toInt - (hours * 60)
      val duration = new LocalTime(hours, mins)
      String.format(JSON_REPLY, operator, flightNumber, origin, destination, jsonDateTime(departureDate), jsonDateTime(arrivalDate),
        JSON_FLIGHT_TIME_FORMATTER.print(duration),
        fares("FIF.1").getCurrencyUnit.toString, fares("FIF.1").getAmount.toPlainString,
        fares("FIF.2").getCurrencyUnit.toString, fares("FIF.2").getAmount.toPlainString,
        fares("FIF.3").getCurrencyUnit.toString, fares("FIF.3").getAmount.toPlainString,
        fares("CIF.1").getCurrencyUnit.toString, fares("CIF.1").getAmount.toPlainString,
        fares("CIF.2").getCurrencyUnit.toString, fares("CIF.2").getAmount.toPlainString,
        fares("CIF.3").getCurrencyUnit.toString, fares("CIF.3").getAmount.toPlainString,
        fares("YIF.1").getCurrencyUnit.toString, fares("YIF.1").getAmount.toPlainString,
        fares("YIF.2").getCurrencyUnit.toString, fares("YIF.2").getAmount.toPlainString,
        fares("YIF.3").getCurrencyUnit.toString, fares("YIF.3").getAmount.toPlainString)
    }

    val JSON_DT_FORMATTER = DateTimeFormat.forPattern("MM-dd-YYYY")
    val JSON_TM_FORMATTER = DateTimeFormat.forPattern("hh:mma")
    val JSON_FLIGHT_TIME_FORMATTER = DateTimeFormat.forPattern("HH:mm")

    def jsonDateTime(dateTime: DateTime) =
      "{\"date\": \"" + JSON_DT_FORMATTER.print(dateTime) + "\", \"time\": \"" + JSON_TM_FORMATTER.print(dateTime) + "\"}"

    val DT_FORMATTER = DateTimeFormat.forPattern("ddMMYY-HH:mm")
    val COBOL_DT_FORMATTER = DateTimeFormat.forPattern("yyMMddHHmm")

    def toCSV = flightNumber + "," + origin + "," + destination + ",\"" +
      DT_FORMATTER.print(departureDate) + "\",\"" +
      DT_FORMATTER.print(arrivalDate) + "\",\"" +
      fares("YIF.1") + ";" + fares("YIF.2") + ";" + fares("YIF.3") + "\""

    /*
    DATA DIVISION.
    FILE SECTION.
    FD  AVAILABILITY
    01  FLIGHT
        05  OPERATOR              PIC X(2).
        05  FLIGHT-NB             PIC X(5).
        05  ORIGIN-AIRPORT        PIC X(4).
        05  DEST-AIRPORT          PIC X(4).
        05  DEPART-DT             PIC 9(10).
        05  ARRIVE-DT             PIC 9(10).
        05  FARE-CLASSES OCCURS 3 TIMES.
            07  TICKET            PIC 999V99.
            07  FEES              PIC 999V99.
            07  TAXES             PIC 999V99.
    */
    def toCardFormat =
      String.format("%-2.2s%-5.5s%4.4s%4.4s%10.10s%10.10s%s%s%s%s%s%s%s%s%s",
        operator, flightNumber, origin, destination,
        COBOL_DT_FORMATTER.print(departureDate),
        COBOL_DT_FORMATTER.print(arrivalDate),
        formatCobolMoney(fares("YIF.1").getAmount, 5),
        formatCobolMoney(fares("YIF.2").getAmount, 5),
        formatCobolMoney(fares("YIF.3").getAmount, 5),
        formatCobolMoney(fares("CIF.2").getAmount, 5),
        formatCobolMoney(fares("CIF.1").getAmount, 5),
        formatCobolMoney(fares("CIF.3").getAmount, 5),
        formatCobolMoney(fares("FIF.1").getAmount, 5),
        formatCobolMoney(fares("FIF.2").getAmount, 5),
        formatCobolMoney(fares("FIF.3").getAmount, 5))

    def formatCobolMoney(amount: BigDecimal, width: Int) =
      String.format("%" + width + "." + width + "s",
        amount.multiply(BigDecimal.TEN).setScale(0, RoundingMode.HALF_EVEN)).replaceAll(" ", "0")

    def compare(other: Flight): Int = departureDate.compareTo(other.departureDate)

    def toXml =
      <Flight>
        <CarrierCode>{operator}</CarrierCode>
        <FlightDesignator>{flightNumber}</FlightDesignator>
        <OriginAirport>{origin}</OriginAirport>
        <DestinationAirport>{destination}</DestinationAirport>
        <DepartureDate>{departureDate}</DepartureDate>
        <ArrivalDate>{arrivalDate}</ArrivalDate>
        <Fares>
          <Fare class='FIF'>
            <BasePrice>{fares("FIF.1")}</BasePrice>
            <Fees>{fares("FIF.2")}</Fees>
            <Tax>{fares("FIF.3")}</Tax>
          </Fare>
          <Fare class='CIF'>
            <BasePrice>{fares("CIF.1")}</BasePrice>
            <Fees>{fares("CIF.2")}</Fees>
            <Tax>{fares("CIF.3")}</Tax>
          </Fare>
          <Fare class='YIF'>
            <BasePrice>{fares("YIF.1")}</BasePrice>
            <Fees>{fares("YIF.2")}</Fees>
            <Tax>{fares("YIF.3")}</Tax>
          </Fare>
        </Fares>
      </Flight>
  }

  val JSON_REPLY =
    "{\"flight\": {\n" +
      "  \"operator\" : \"%s\",\n" +
      "  \"flightNumber\" : \"%s\",\n" +
      "  \"departsFrom\" : \"%s\",\n" +
      "  \"arrivesAt\" : \"%s\",\n" +
      "  \"departsOn\" : %s,\n" +
      "  \"arrivesOn\" : %s,\n" +
      "  \"flightTime\" : \"%s\",\n" +
      "  \"farePrices\" : {\n" +
      "    \"first\" : {\n" +
      "      \"ticket\" : { \"currency\" : \"%s\", \"amount\" : %s},\n" +
      "      \"bookingFee\" : { \"currency\" : \"%s\", \"amount\" : %s},\n" +
      "      \"tax\" : { \"currency\" : \"%s\", \"amount\" : %s}\n" +
      "    },\n" +
      "    \"business\" : {\n" +
      "      \"ticket\" : { \"currency\" : \"%s\", \"amount\" : %s},\n" +
      "      \"bookingFee\" : { \"currency\" : \"%s\", \"amount\" : %s},\n" +
      "      \"tax\" : { \"currency\" : \"%s\", \"amount\" : %s}\n" +
      "    },\n" +
      "    \"economy\" : {\n" +
      "      \"ticket\" : { \"currency\" : \"%s\", \"amount\" : %s},\n" +
      "      \"bookingFee\" : { \"currency\" : \"%s\", \"amount\" : %s},\n" +
      "      \"tax\" : { \"currency\" : \"%s\", \"amount\" : %s}\n" +
      "    }\n" +
      "  }\n" +
      " }\n" +
      "}"

}
