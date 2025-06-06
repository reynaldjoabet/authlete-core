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

/** @param parameters
  *   OAuth 2.0 token revocation request parameters which are the request
  *   parameters that the OAuth 2.0 token revocation endpoint ([RFC
  *   7009](https://datatracker.ietf.org/doc/html/rfc7009)) of the authorization
  *   server implementation received from the client application. The value of
  *   parameters is the entire entity body (which is formatted in
  *   `application/x-www-form-urlencoded`) of the request from the client
  *   application.
  * @param clientId
  *   The client ID extracted from `Authorization` header of the revocation
  *   request from the client application. If the revocation endpoint of the
  *   authorization server implementation supports Basic Authentication as a
  *   means of client authentication, and the request from the client
  *   application contains its client ID in `Authorization` header, the value
  *   should be extracted and set to this parameter.
  * @param clientSecret
  *   The client secret extracted from `Authorization` header of the revocation
  *   request from the client application. If the revocation endpoint of the
  *   authorization server implementation supports basic authentication as a
  *   means of client authentication, and the request from the client
  *   application contained its client secret in `Authorization` header, the
  *   value should be extracted and set to this parameter.
  * @param clientCertificate
  *   The client certificate used in the TLS connection between the client
  *   application and the revocation endpoint.
  * @param clientCertificatePath
  *   The certificate path presented by the client during client authentication.
  */
case class RevocationRequest(
    parameters: String,
    clientId: Option[String] = None,
    clientSecret: Option[String] = None,
    clientCertificate: Option[String] = None,
    clientCertificatePath: Option[String] = None
)

object RevocationRequest {

  given jsonCodec: JsonValueCodec[RevocationRequest] =
    JsonCodecMaker.make(codecMakerConfig)

  given encoderRevocationRequest: Encoder[RevocationRequest] =
    Encoder.instance { t =>
      Json.fromFields {
        Seq(
          Some("parameters" -> t.parameters.asJson),
          t.clientId.map(v => "clientId" -> v.asJson),
          t.clientSecret.map(v => "clientSecret" -> v.asJson),
          t.clientCertificate.map(v => "clientCertificate" -> v.asJson),
          t.clientCertificatePath.map(v => "clientCertificatePath" -> v.asJson)
        ).flatten
      }
    }

  given decoderRevocationRequest: Decoder[RevocationRequest] =
    Decoder.instance { c =>
      for {
        parameters <- c.downField("parameters").as[String]
        clientId <- c.downField("clientId").as[Option[String]]
        clientSecret <- c.downField("clientSecret").as[Option[String]]
        clientCertificate <- c.downField("clientCertificate").as[Option[String]]
        clientCertificatePath <- c
          .downField("clientCertificatePath")
          .as[Option[String]]
      } yield RevocationRequest(
        parameters = parameters,
        clientId = clientId,
        clientSecret = clientSecret,
        clientCertificate = clientCertificate,
        clientCertificatePath = clientCertificatePath
      )
    }

}
