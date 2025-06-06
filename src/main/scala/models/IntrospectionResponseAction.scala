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

/** The next action that the authorization server implementation should take.
  */
enum IntrospectionResponseAction(val value: String)
    derives ConfiguredJsonValueCodec {

  case INTERNALSERVERERROR
      extends IntrospectionResponseAction("INTERNAL_SERVER_ERROR")
  case BADREQUEST extends IntrospectionResponseAction("BAD_REQUEST")
  case UNAUTHORIZED extends IntrospectionResponseAction("UNAUTHORIZED")
  case FORBIDDEN extends IntrospectionResponseAction("FORBIDDEN")
  case OK extends IntrospectionResponseAction("OK")

}

object IntrospectionResponseAction {

  def withValueOpt(value: String): Option[IntrospectionResponseAction] =
    IntrospectionResponseAction.values
      .find(_.value == value)

  def withValue(value: String): IntrospectionResponseAction =
    withValueOpt(value).getOrElse(
      throw java.lang
        .IllegalArgumentException(
          s"IntrospectionResponseAction enum case not found: $value"
        )
    )

  given decoderIntrospectionResponseAction
      : Decoder[IntrospectionResponseAction] = Decoder.decodeString
    .map(withValue)

  given encoderIntrospectionResponseAction
      : Encoder[IntrospectionResponseAction] = Encoder.encodeString
    .contramap[IntrospectionResponseAction](_.value)

}
