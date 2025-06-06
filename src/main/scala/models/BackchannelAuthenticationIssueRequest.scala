/** Authlete API Authlete API Document.
  *
  * The version of the OpenAPI document: 2.3.12 Contact: team@openapitools.org
  *
  * NOTE: This class is auto generated by OpenAPI Generator
  * (https://openapi-generator.tech). https://openapi-generator.tech Do not edit
  * the class manually.
  */
package authlete
package models

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.ConfiguredJsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import io.circe.{Decoder, Encoder, Json}
import io.circe.syntax.*

/** @param ticket
  *   The ticket issued from Authlete's `/backchannel/authentication` API.
  */
case class BackchannelAuthenticationIssueRequest(
    ticket: String
)

object BackchannelAuthenticationIssueRequest {

  given jsonCodec: JsonValueCodec[BackchannelAuthenticationIssueRequest] =
    JsonCodecMaker.make(codecMakerConfig)

  given encoderBackchannelAuthenticationIssueRequest
      : Encoder[BackchannelAuthenticationIssueRequest] = Encoder.instance { t =>
    Json.fromFields {
      Seq(
        Some("ticket" -> t.ticket.asJson)
      ).flatten
    }
  }

  given decoderBackchannelAuthenticationIssueRequest
      : Decoder[BackchannelAuthenticationIssueRequest] = Decoder.instance { c =>
    for {
      ticket <- c.downField("ticket").as[String]
    } yield BackchannelAuthenticationIssueRequest(
      ticket = ticket
    )
  }

}
