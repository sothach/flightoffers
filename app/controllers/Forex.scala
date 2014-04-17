package controllers

import play.api.mvc._
import play.api.libs.json.Json
import org.joda.money.CurrencyUnit

object Forex extends Controller {

  def getRate(curr1: String, curr2: String) = Action { request =>
    Security.authorized(request) match {
      case Some(true) => Ok(Json.toJson(getRateForPair(curr1,curr2)))
      case Some(false) | None => Unauthorized
    }
  }

  def getRateForPair ( currency1: String, currency2: String) = Map (
    "currency1" -> currency1,
    "currency2" -> currency2,
    "rate" -> exchangeRates((CurrencyUnit.of(currency1),CurrencyUnit.of(currency2))).toString)

  val exchangeRates = Map (
    (CurrencyUnit.EUR, CurrencyUnit.USD) -> 1.38,
    (CurrencyUnit.EUR, CurrencyUnit.GBP) -> 0.84,
    (CurrencyUnit.EUR, CurrencyUnit.of("TRY")) -> 3.08,
    (CurrencyUnit.USD, CurrencyUnit.EUR) -> 0.72,
    (CurrencyUnit.USD, CurrencyUnit.GBP) -> 0.61,
    (CurrencyUnit.USD, CurrencyUnit.of("TRY")) -> 2.23,
    (CurrencyUnit.GBP, CurrencyUnit.EUR) -> 1.20,
    (CurrencyUnit.GBP, CurrencyUnit.USD) -> 1.65,
    (CurrencyUnit.GBP, CurrencyUnit.of("TRY")) -> 3.08,
    (CurrencyUnit.of("TRY"), CurrencyUnit.EUR) -> 0.32,
    (CurrencyUnit.of("TRY"), CurrencyUnit.USD) -> 0.45,
    (CurrencyUnit.of("TRY"), CurrencyUnit.GBP) -> 0.27)


}
