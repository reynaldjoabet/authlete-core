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
enum DeviceCompleteResponseAction(val value: String)
    derives ConfiguredJsonValueCodec {

  case SERVERERROR extends DeviceCompleteResponseAction("SERVER_ERROR")
  case USERCODENOTEXIST
      extends DeviceCompleteResponseAction("USER_CODE_NOT_EXIST")
  case USERCODEEXPIRED extends DeviceCompleteResponseAction("USER_CODE_EXPIRED")
  case INVALIDREQUEST extends DeviceCompleteResponseAction("INVALID_REQUEST")
  case SUCCESS extends DeviceCompleteResponseAction("SUCCESS")

}

object DeviceCompleteResponseAction {

  def withValueOpt(value: String): Option[DeviceCompleteResponseAction] =
    DeviceCompleteResponseAction.values.find(_.value == value)

  def withValue(value: String): DeviceCompleteResponseAction =
    withValueOpt(value).getOrElse(
      throw java.lang
        .IllegalArgumentException(
          s"DeviceCompleteResponseAction enum case not found: $value"
        )
    )

  given decoderDeviceCompleteResponseAction
      : Decoder[DeviceCompleteResponseAction] = Decoder.decodeString
    .map(withValue)

  given encoderDeviceCompleteResponseAction
      : Encoder[DeviceCompleteResponseAction] = Encoder.encodeString
    .contramap[DeviceCompleteResponseAction](_.value)

}
