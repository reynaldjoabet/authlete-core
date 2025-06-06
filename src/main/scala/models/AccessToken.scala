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

/** @param accessTokenHash
  *   The hash of the access token.
  * @param accessTokenExpiresAt
  *   The timestamp at which the access token will expire.
  * @param refreshTokenHash
  *   The hash of the refresh token.
  * @param refreshTokenExpiresAt
  *   The timestamp at which the refresh token will expire.
  * @param createdAt
  *   The timestamp at which the access token was first created.
  * @param lastRefreshedAt
  *   The timestamp at which the access token was last refreshed using the
  *   refresh token.
  * @param clientId
  *   The ID of the client associated with the access token.
  * @param subject
  *   The subject (= unique user ID) associated with the access token.
  * @param grantType
  * @param scopes
  *   The scopes associated with the access token.
  * @param properties
  *   The properties associated with the access token.
  */
case class AccessToken(
    accessTokenHash: Option[String] = None,
    accessTokenExpiresAt: Option[Long] = None,
    refreshTokenHash: Option[String] = None,
    refreshTokenExpiresAt: Option[Long] = None,
    createdAt: Option[Long] = None,
    lastRefreshedAt: Option[Long] = None,
    clientId: Option[Long] = None,
    subject: Option[String] = None,
    grantType: Option[GrantType] = None,
    scopes: Option[Seq[String]] = None,
    properties: Option[Seq[Property]] = None
)

object AccessToken {

  given jsonCodec: JsonValueCodec[AccessToken] =
    JsonCodecMaker.make(codecMakerConfig)

  given encoderAccessToken: Encoder[AccessToken] = Encoder.instance { t =>
    Json.fromFields {
      Seq(
        t.accessTokenHash.map(v => "accessTokenHash" -> v.asJson),
        t.accessTokenExpiresAt.map(v => "accessTokenExpiresAt" -> v.asJson),
        t.refreshTokenHash.map(v => "refreshTokenHash" -> v.asJson),
        t.refreshTokenExpiresAt.map(v => "refreshTokenExpiresAt" -> v.asJson),
        t.createdAt.map(v => "createdAt" -> v.asJson),
        t.lastRefreshedAt.map(v => "lastRefreshedAt" -> v.asJson),
        t.clientId.map(v => "clientId" -> v.asJson),
        t.subject.map(v => "subject" -> v.asJson),
        t.grantType.map(v => "grantType" -> v.asJson),
        t.scopes.map(v => "scopes" -> v.asJson),
        t.properties.map(v => "properties" -> v.asJson)
      ).flatten
    }
  }

  given decoderAccessToken: Decoder[AccessToken] = Decoder.instance { c =>
    for {
      accessTokenHash <- c.downField("accessTokenHash").as[Option[String]]
      accessTokenExpiresAt <- c
        .downField("accessTokenExpiresAt")
        .as[Option[Long]]
      refreshTokenHash <- c.downField("refreshTokenHash").as[Option[String]]
      refreshTokenExpiresAt <- c
        .downField("refreshTokenExpiresAt")
        .as[Option[Long]]
      createdAt <- c.downField("createdAt").as[Option[Long]]
      lastRefreshedAt <- c.downField("lastRefreshedAt").as[Option[Long]]
      clientId <- c.downField("clientId").as[Option[Long]]
      subject <- c.downField("subject").as[Option[String]]
      grantType <- mapEmptyStringToNull(c.downField("grantType"))
        .as[Option[GrantType]]
      scopes <- c.downField("scopes").as[Option[Seq[String]]]
      properties <- c.downField("properties").as[Option[Seq[Property]]]
    } yield AccessToken(
      accessTokenHash = accessTokenHash,
      accessTokenExpiresAt = accessTokenExpiresAt,
      refreshTokenHash = refreshTokenHash,
      refreshTokenExpiresAt = refreshTokenExpiresAt,
      createdAt = createdAt,
      lastRefreshedAt = lastRefreshedAt,
      clientId = clientId,
      subject = subject,
      grantType = grantType,
      scopes = scopes,
      properties = properties
    )
  }

}
