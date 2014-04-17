package model

import org.joda.time.DateTime

case class IATAAirport(code: String)

case class FlightProduct(origin: IATAAirport, destination: IATAAirport, departureDate: DateTime, arrivalDate: DateTime)

