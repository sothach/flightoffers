package controllers

import play.api.mvc._
import scala.collection.mutable.{Map => MMap}
import java.math.BigInteger
import play.api.libs.iteratee.Enumerator
import java.util.UUID
import play.api.Play.current

object Security extends Controller {

  def login = Action { request =>
    request.body.asJson match {
      case Some(json) =>
        loginUser((json \ "user").as[String], (json \ "pass").as[String]) match {
          case Right(token) =>
            SimpleResult(
              header = ResponseHeader(200, Map(AUTHORIZATION -> token)),
              body = Enumerator.empty[Array[Byte]])
          case Left(error) => BadRequest(error)
        }
      case None => BadRequest("missing authentication data")
    }
  }

  private[controllers]
  def authorized(request: Request[AnyContent]) =
    for {token <- request.headers.get(AUTHORIZATION)
         auth = tokens.contains(token)
    } yield auth

  private val tokens = MMap[String, (String, String)]()

  def loginUser(user: String, password: String): Either[String, String] = {
    val token = createToken
    tokens(token) = (user, password)
    Right(token)
  }

  private def createToken = {
    val token = UUID.randomUUID.toString.replaceAll("-", "")
    val bigInt = new BigInteger(token, 16)
    bigInt.toString(36).toUpperCase
  }
}
