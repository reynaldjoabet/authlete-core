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

/** @param token
  *   An access token to introspect.
  * @param scopes
  *   A string array listing names of scopes which the caller (= a protected
  *   resource endpoint of the service) requires. When the content type of the
  *   request from the service is `application/x-www-form-urlencoded`, the
  *   format of `scopes` is a space-separated list of scope names. If this
  *   parameter is a non-empty array and if it contains a scope which is not
  *   covered by the access token,`action=FORBIDDEN` with
  *   `error=insufficient_scope` is returned from Authlete.
  * @param subject
  *   A subject (= a user account managed by the service) whom the caller (= a
  *   protected resource endpoint of the service) requires. If this parameter is
  *   not `null` and if the value does not match the subject who is associated
  *   with the access token, `action=FORBIDDEN` with `error=invalid_request` is
  *   returned from Authlete.
  * @param clientCertificate
  *   Client certificate in PEM format, used to validate binding against access
  *   tokens using the TLS client certificate confirmation method.
  * @param dpop
  *   `DPoP` header presented by the client during the request to the resource
  *   server. The header contains a signed JWT which includes the public key
  *   that is paired with the private key used to sign the JWT. See [OAuth 2.0
  *   Demonstration of Proof-of-Possession at the Application Layer
  *   (DPoP)](https://datatracker.ietf.org/doc/html/draft-ietf-oauth-dpop) for
  *   details.
  * @param htm
  *   HTTP method of the request from the client to the protected resource
  *   endpoint. This field is used to validate the `DPoP` header. See [OAuth 2.0
  *   Demonstration of Proof-of-Possession at the Application Layer
  *   (DPoP)](https://datatracker.ietf.org/doc/html/draft-ietf-oauth-dpop) for
  *   details.
  * @param htu
  *   URL of the protected resource endpoint. This field is used to validate the
  *   `DPoP` header. See [OAuth 2.0 Demonstration of Proof-of-Possession at the
  *   Application Layer
  *   (DPoP)](https://datatracker.ietf.org/doc/html/draft-ietf-oauth-dpop) for
  *   details.
  * @param resources
  *   The resources specified by the `resource` request parameters in the token
  *   request. See \"Resource Indicators for OAuth 2.0\" for details.
  * @param acrValues
  *   Authentication Context Class Reference values one of which the user
  *   authentication performed during the course of issuing the access token
  *   must satisfy.
  * @param maxAge
  *   The maximum authentication age which is the maximum allowable elapsed time
  *   since the user authentication was performed during the course of issuing
  *   the access token.
  * @param requiredComponents
  *   HTTP Message Components required to be in the signature. If absent,
  *   defaults to [ \"@method\", \"@target-uri\", \"authorization\" ].
  * @param uri
  *   The full URL of the userinfo endpoint.
  * @param message
  *   The HTTP message body of the request, if present.
  * @param headers
  *   HTTP headers to be included in processing the signature. If this is a
  *   signed request, this must include the Signature and Signature-Input
  *   headers, as well as any additional headers covered by the signature.
  */
case class IntrospectionRequest(
    token: String,
    scopes: Option[Seq[String]] = None,
    subject: Option[String] = None,
    clientCertificate: Option[String] = None,
    dpop: Option[String] = None,
    htm: Option[String] = None,
    htu: Option[String] = None,
    resources: Option[Seq[String]] = None,
    acrValues: Option[Seq[String]] = None,
    maxAge: Option[Long] = None,
    requiredComponents: Option[Seq[String]] = None,
    uri: Option[String] = None,
    message: Option[String] = None,
    headers: Option[Seq[Pair]] = None
)

object IntrospectionRequest {

  given jsonCodec: JsonValueCodec[IntrospectionRequest] =
    JsonCodecMaker.make(codecMakerConfig)

  given encoderIntrospectionRequest: Encoder[IntrospectionRequest] =
    Encoder.instance { t =>
      Json.fromFields {
        Seq(
          Some("token" -> t.token.asJson),
          t.scopes.map(v => "scopes" -> v.asJson),
          t.subject.map(v => "subject" -> v.asJson),
          t.clientCertificate.map(v => "clientCertificate" -> v.asJson),
          t.dpop.map(v => "dpop" -> v.asJson),
          t.htm.map(v => "htm" -> v.asJson),
          t.htu.map(v => "htu" -> v.asJson),
          t.resources.map(v => "resources" -> v.asJson),
          t.acrValues.map(v => "acrValues" -> v.asJson),
          t.maxAge.map(v => "maxAge" -> v.asJson),
          t.requiredComponents.map(v => "requiredComponents" -> v.asJson),
          t.uri.map(v => "uri" -> v.asJson),
          t.message.map(v => "message" -> v.asJson),
          t.headers.map(v => "headers" -> v.asJson)
        ).flatten
      }
    }

  given decoderIntrospectionRequest: Decoder[IntrospectionRequest] =
    Decoder.instance { c =>
      for {
        token <- c.downField("token").as[String]
        scopes <- c.downField("scopes").as[Option[Seq[String]]]
        subject <- c.downField("subject").as[Option[String]]
        clientCertificate <- c.downField("clientCertificate").as[Option[String]]
        dpop <- c.downField("dpop").as[Option[String]]
        htm <- c.downField("htm").as[Option[String]]
        htu <- c.downField("htu").as[Option[String]]
        resources <- c.downField("resources").as[Option[Seq[String]]]
        acrValues <- c.downField("acrValues").as[Option[Seq[String]]]
        maxAge <- c.downField("maxAge").as[Option[Long]]
        requiredComponents <- c
          .downField("requiredComponents")
          .as[Option[Seq[String]]]
        uri <- c.downField("uri").as[Option[String]]
        message <- c.downField("message").as[Option[String]]
        headers <- c.downField("headers").as[Option[Seq[Pair]]]
      } yield IntrospectionRequest(
        token = token,
        scopes = scopes,
        subject = subject,
        clientCertificate = clientCertificate,
        dpop = dpop,
        htm = htm,
        htu = htu,
        resources = resources,
        acrValues = acrValues,
        maxAge = maxAge,
        requiredComponents = requiredComponents,
        uri = uri,
        message = message,
        headers = headers
      )
    }

}
