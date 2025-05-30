package authlete

import java.net.URI
import java.time.{Instant, LocalDate, LocalDateTime, OffsetDateTime}
import java.util.UUID

import scala.util.Try

import cats.effect.kernel.implicits
import cats.implicits.catsSyntaxEither

import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReader, JsonWriter}
import com.github.plokhotnyuk.jsoniter_scala.macros.CodecMakerConfig
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import io.circe.{ACursor, Decoder, Encoder, Json}
import io.circe.syntax.* // import for asJson extension
import org.http4s.headers.`Content-Type`
import org.http4s.{
  DecodeFailure,
  DecodeResult,
  EntityDecoder,
  EntityEncoder,
  InvalidMessageBodyFailure,
  MalformedMessageBodyFailure,
  MediaRange,
  MediaType,
  Uri
}
import com.github.plokhotnyuk.jsoniter_scala.circe.JsoniterScalaCodec.*
import sttp.model.StatusCode
import cats.effect.IO
import io.circe.JsoniterScalaCodec

package object models {

  trait CirceInstances {

    private val defaultWriterConfig: WriterConfig =
      WriterConfig.withPreferredBufSize(100 * 1024)

    def jsonEncoderOf[A: Encoder]: EntityEncoder[IO, A] =
      EntityEncoder
        .byteArrayEncoder[IO]
        .contramap[A] { value =>
          writeToArray(value.asJson, defaultWriterConfig)
        }
        .withContentType(`Content-Type`(MediaType.application.json))
  }
  given decodeUUID: Decoder[UUID] =
    Decoder.decodeString.map(str => UUID.fromString(str))

  given encodeUUID: Encoder[UUID] =
    Encoder.encodeString.contramap[UUID](uuid => uuid.toString)

  given decodeInstant: Decoder[Instant] =
    Decoder.decodeString.map(str => OffsetDateTime.parse(str).toInstant)

  given encodeInstant: Encoder[Instant] =
    Encoder.encodeString.contramap[Instant](_.toString)

  given decodeLocalDate: Decoder[LocalDate] =
    Decoder.decodeString.map(str => LocalDate.parse(str))

  given encodeLocalDate: Encoder[LocalDate] =
    Encoder.encodeString.contramap[LocalDate](_.toString)

  given decodeJson: Decoder[Json] =
    Decoder.decodeString.map(str => Json.fromString(str))

  given encodeJson: Encoder[Json] =
    Encoder.encodeString.contramap[Json](_.toString)

  def mapEmptyStringToNull(f: ACursor): ACursor =
    f.withFocus(json => if json.toString == "\"\"" then Json.Null else json)

  given stringCodec: JsonValueCodec[String] =
    JsonCodecMaker.make(codecMakerConfig)

//implicit def arrayCodec[A: JsonValueCodec]: JsonValueCodec[Array[A]] = JsonCodecMaker.make(codecMakerConfig)

  implicit def listCodec[A: JsonValueCodec]: JsonValueCodec[List[A]] =
    JsonCodecMaker.make(codecMakerConfig)

  given mapStringStringCodec: JsonValueCodec[Map[String, String]] =
    JsonCodecMaker.make(codecMakerConfig)

  given localDateCodec: JsonValueCodec[LocalDate] =
    JsonCodecMaker.make(codecMakerConfig)

  given localDateTimeCodec: JsonValueCodec[LocalDateTime] =
    JsonCodecMaker.make(codecMakerConfig)

  given uriCodec: JsonValueCodec[URI] = new JsonValueCodec[URI] {
    override def decodeValue(in: JsonReader, default: URI): URI =
      URI.create(in.readString(null))
    override def encodeValue(x: URI, out: JsonWriter): Unit =
      out.writeVal(x.toString)
    override def nullValue: URI = null
  }

  // implicit val codecURI: JsonValueCodec[URI] = new JsonValueCodec[URI] {
  //     val nullValue: URI = null
  //     def encodeValue(x: URI, out: JsonWriter): Unit = out.writeVal(x.toString)
  //     def decodeValue(in: JsonReader, default: URI): URI =
  //       if (in.isNextToken('"')) {
  //         in.rollbackToken()
  //         Try(URI.create(in.readString(""))).toOption.getOrElse(nullValue)
  //       } else {
  //         in.rollbackToken()
  //         nullValue
  //       }
  //   }

  inline given codecMakerConfig: CodecMakerConfig =
    CodecMakerConfig
      .withAllowRecursiveTypes(true)
      .withDiscriminatorFieldName(None)
      .withIsStringified(true)
      // .withAdtLeafClassNameMapper(
      //   JsonCodecMaker.simpleClassName.andThen(JsonCodecMaker.enforce_snake_case)
      // )
      .withAdtLeafClassNameMapper(
        JsonCodecMaker.simpleClassName(
          _
        ) // JsonCodecMaker.enforce_snake_case//.andThen(_.toUpperCase())
      )
      .withRequireDiscriminatorFirst(false)
      .withMapAsArray(true)
      .withFieldNameMapper(JsonCodecMaker.enforceCamelCase)

  /** A wrapper for JSON parsing errors.
    */
  opaque type ParsingError = String
  object ParsingError {

    def apply(throwable: Throwable): ParsingError =
      s"JSON parsing error due to $throwable"

  }

  trait JsoniterSyntaticSugar {

    extension (payload: String) {

      /** Deserializes `A` from the provided JSON string.
        */
      def fromJson[A](using JsonValueCodec[A]): Either[ParsingError, A] =
        Try(readFromString[A](payload)).toEither.leftMap(ParsingError.apply)

    }

    extension [A](obj: A) {

      /** Serializes `A` to a JSON string.
        */
      def toJson(using JsonValueCodec[A]): String = writeToString[A](obj)
    }

  }

  object JsoniterSyntaticSugar extends JsoniterSyntaticSugar

}
