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

import scala.collection.immutable.Seq

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.ConfiguredJsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import io.circe.{Decoder, Encoder, Json}
import io.circe.syntax.*

/** @param resultCode
  *   The code which represents the result of the API call.
  * @param resultMessage
  *   A short message which explains the result of the API call.
  * @param action
  * @param accessToken
  *   The access token which has been specified by the request.
  * @param accessTokenExpiresAt
  *   The date at which the access token will expire.
  * @param properties
  *   The extra properties associated with the access token.
  * @param scopes
  *   The scopes associated with the access token.
  * @param authorizationDetails
  * @param tokenType
  *   The token type associated with the access token.
  * @param forExternalAttachment
  *   the flag which indicates whether the access token is for an external
  *   attachment.
  * @param refreshTokenExpiresAt
  *   The date at which the refresh token will expire.
  */
case class TokenUpdateResponse(
    resultCode: Option[String] = None,
    resultMessage: Option[String] = None,
    action: Option[TokenUpdateResponseAction] = None,
    accessToken: Option[String] = None,
    accessTokenExpiresAt: Option[Long] = None,
    properties: Option[Seq[Property]] = None,
    scopes: Option[Seq[String]] = None,
    authorizationDetails: Option[AuthzDetails] = None,
    tokenType: Option[String] = None,
    forExternalAttachment: Option[Boolean] = None,
    refreshTokenExpiresAt: Option[Long] = None
)

object TokenUpdateResponse {

  given jsonCodec: JsonValueCodec[TokenUpdateResponse] =
    JsonCodecMaker.make(codecMakerConfig)

  given encoderTokenUpdateResponse: Encoder[TokenUpdateResponse] =
    Encoder.instance { t =>
      Json.fromFields {
        Seq(
          t.resultCode.map(v => "resultCode" -> v.asJson),
          t.resultMessage.map(v => "resultMessage" -> v.asJson),
          t.action.map(v => "action" -> v.asJson),
          t.accessToken.map(v => "accessToken" -> v.asJson),
          t.accessTokenExpiresAt.map(v => "accessTokenExpiresAt" -> v.asJson),
          t.properties.map(v => "properties" -> v.asJson),
          t.scopes.map(v => "scopes" -> v.asJson),
          t.authorizationDetails.map(v => "authorizationDetails" -> v.asJson),
          t.tokenType.map(v => "tokenType" -> v.asJson),
          t.forExternalAttachment.map(v => "forExternalAttachment" -> v.asJson),
          t.refreshTokenExpiresAt.map(v => "refreshTokenExpiresAt" -> v.asJson)
        ).flatten
      }
    }

  given decoderTokenUpdateResponse: Decoder[TokenUpdateResponse] =
    Decoder.instance { c =>
      for {
        resultCode <- c.downField("resultCode").as[Option[String]]
        resultMessage <- c.downField("resultMessage").as[Option[String]]
        action <- mapEmptyStringToNull(c.downField("action"))
          .as[Option[TokenUpdateResponseAction]]
        accessToken <- c.downField("accessToken").as[Option[String]]
        accessTokenExpiresAt <- c
          .downField("accessTokenExpiresAt")
          .as[Option[Long]]
        properties <- c.downField("properties").as[Option[Seq[Property]]]
        scopes <- c.downField("scopes").as[Option[Seq[String]]]
        authorizationDetails <- c
          .downField("authorizationDetails")
          .as[Option[AuthzDetails]]
        tokenType <- c.downField("tokenType").as[Option[String]]
        forExternalAttachment <- c
          .downField("forExternalAttachment")
          .as[Option[Boolean]]
        refreshTokenExpiresAt <- c
          .downField("refreshTokenExpiresAt")
          .as[Option[Long]]
      } yield TokenUpdateResponse(
        resultCode = resultCode,
        resultMessage = resultMessage,
        action = action,
        accessToken = accessToken,
        accessTokenExpiresAt = accessTokenExpiresAt,
        properties = properties,
        scopes = scopes,
        authorizationDetails = authorizationDetails,
        tokenType = tokenType,
        forExternalAttachment = forExternalAttachment,
        refreshTokenExpiresAt = refreshTokenExpiresAt
      )
    }

}
