package authlete.services
import authlete.models.Result
import config.AuthleteConfig
import org.typelevel.log4cats.Logger
import sttp.client4.*
import sttp.client4._
import sttp.client4.jsoniter.*
import sttp.client4.Backend
import sttp.client4.DefaultFutureBackend
import sttp.model.MediaType
import sttp.model.Method
import sttp.model.StatusCode
import sttp.model.Uri
import authlete.models.ClientRegistrationUpdateRequest
import authlete.models.BackchannelAuthenticationCompleteResponse
import authlete.models.ServiceJwksGetResponse
import authlete.models.TokenGetListResponse
import authlete.models.AuthorizationIssueResponse
import authlete.models.TokenFailResponse
import authlete.models.UserinfoRequest
import authlete.models.ClientRegistrationGetResponse
import authlete.models.HskCreateResponse
import authlete.models.HskGetResponse
import authlete.models.DeviceVerificationRequest
import authlete.models.TokenUpdateRequest
import authlete.models.AuthorizationIssueRequest
import authlete.models.TokenRevokeRequest
import authlete.models.TokenRequest
import authlete.models.StandardIntrospectionRequest
import authlete.models.ClientRegistrationDeleteResponse
import authlete.models.DeviceCompleteResponse
import authlete.models.JoseVerifyResponse
import authlete.models.BackchannelAuthenticationIssueRequest
import authlete.models.ClientRegistrationUpdateResponse
import authlete.models.PushedAuthorizationResponse
import authlete.models.GMRequest
import authlete.models.IntrospectionRequest
import authlete.models.BackchannelAuthenticationCompleteRequest
import authlete.models.InfoResponse
import authlete.models.BackchannelAuthenticationRequest
import authlete.models.ClientRegistrationGetRequest
import authlete.models.HskCreateRequest
import authlete.models.AuthorizationResponse
import authlete.models.DeviceAuthorizationResponse
import authlete.models.ClientExtensionRequestableScopesUpdateRequest
import authlete.models.ClientExtensionRequestableScopesUpdateResponse
import authlete.models.RevocationRequest
import authlete.models.TokenCreateResponse
import authlete.models.TokenIssueRequest
import authlete.models.TokenFailRequest
import authlete.models.AuthorizationFailResponse
import authlete.models.ClientExtensionRequestableScopesGetResponse
import authlete.models.PushedAuthorizationRequest
import authlete.models.HskGetListResponse
import authlete.models.BackchannelAuthenticationFailResponse
import authlete.models.TokenCreateRequest
import authlete.models.GMResponse
import authlete.models.BackchannelAuthenticationFailRequest
import authlete.models.HskDeleteResponse
import authlete.models.BackchannelAuthenticationResponse
import authlete.models.IdtokenReissueResponse
import authlete.models.BackchannelAuthenticationIssueResponse
import authlete.models.RevocationResponse
import authlete.models.ClientRegistrationRequest
import authlete.models.IntrospectionResponse
import authlete.models.TokenUpdateResponse
import authlete.models.IdtokenReissueRequest
import authlete.models.UserinfoResponse
import authlete.models.DeviceAuthorizationRequest
import authlete.models.ClientRegistrationDeleteRequest
import authlete.models.UserinfoIssueResponse
import authlete.models.DeviceVerificationResponse
import authlete.models.TokenIssueResponse
import authlete.models.JoseVerifyRequest
import authlete.models.TokenResponse
import authlete.models.StandardIntrospectionResponse
import authlete.models.UserinfoIssueRequest
import authlete.models.DeviceCompleteRequest
import authlete.models.TokenRevokeResponse
import authlete.models.ClientRegistrationResponse
import authlete.models.AuthorizationRequest
import authlete.models.AuthorizationFailRequest
import sttp.client4.jsoniter.*
import authlete.models.JsoniterSyntaticSugar.*
import cats.syntax.all.*
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import cats.Monad
import cats.effect.kernel.Concurrent
import cats.effect.syntax.all.*
import cats.effect.kernel.syntax.resource
import com.github.plokhotnyuk.jsoniter_scala.core.*
import authlete.models.FailedRequest
import org.http4s.Response
final class AuthleteClient[F[*]: Concurrent](
    backend: Backend[F],
    config: AuthleteConfig,
    logger: Logger[F]
) extends AuthleteService[F] {

  private val basicReq = basicRequest.auth
    // .basic(config.apiKey, config.apiSecret)// V3 API requires an access token, not a key and secret
    .bearer(config.auth)
    // .contentType(MediaType.ApplicationJson)
    .readTimeout(config.requestTimeout)

  private val baseUri = Uri(config.baseUrl)

  private def postRequest[B: JsonValueCodec, R: JsonValueCodec](
      uri: Uri,
      body: B
  ) =
    basicReq
      .post(uri)
      .body(body.toJson)
      .contentType(MediaType.ApplicationJson)
      .response(asJson[R])

  private def postRequestWithUnitResponse[B: JsonValueCodec](
      uri: Uri,
      body: B
  ) =
    basicReq
      .post(uri)
      .body(body.toJson)
      .contentType(MediaType.ApplicationJson)
      .response(ignore)

  private def postRequest[R: JsonValueCodec](uri: Uri) =
    basicReq.post(uri).response(asJson[R])

  private def postRequestWithUnitResponse(uri: Uri) =
    basicReq.post(uri).response(ignore)

  private def putRequest[B: JsonValueCodec, R: JsonValueCodec](
      uri: Uri,
      body: B
  ) =
    basicReq
      .put(uri)
      .body(body.toJson)
      .contentType(MediaType.ApplicationJson)
      .response(asJson[R])

  // If you set the body as a Map[String, String] or Seq[(String, String)], it will be encoded as form-data (as if a web form with the given values was submitted)
  private def postRequestForm[R: JsonValueCodec](
      uri: Uri,
      body: Map[String, String]
  ) =
    basicReq
      .post(uri)
      .body(body)
      .response(asJson[R])
      .contentType(MediaType.ApplicationXWwwFormUrlencoded)

  private def getRequest[R: JsonValueCodec](uri: Uri) =
    basicReq.get(uri).response(asJson[R])

  private def deleteRequestWithUnitResponse(uri: Uri) =
    basicReq.delete(uri).response(ignore)

  private def deleteRequest[R: JsonValueCodec](uri: Uri) =
    basicReq.delete(uri).response(asJson[R])

  import cats.syntax.all._
  import cats.effect.Concurrent
  import com.github.plokhotnyuk.jsoniter_scala.core._

  override def authAuthorization(
      authorizationRequest: AuthorizationRequest
  ): F[AuthorizationResponse] = {
    val uri =
      baseUri.withPath(List("api", config.apiKey, "auth", "authorization"))

    basicReq
      .post(uri)
      .contentType("application/json")
      .body(authorizationRequest.toJson)
      .contentType(MediaType.ApplicationJson)
      .send(backend) // F[Response[Either[String, String]]]
      // .flatTap(resp =>
      //            logger.info(s"Authorization response  with action ${resp.action} ,result code ${resp
      //                .resultCode} and message ${resp.resultMessage}")
      //          )
      .flatMap { response =>
        response.body match {
          case Right(body) =>
            response.code.code match {
              case 200 =>
                Either
                  .catchOnly[Throwable](
                    readFromString[AuthorizationResponse](body)
                  )
                  .liftTo[F]

              case 400 | 401 | 403 | 500 =>
                for {
                  res <- Either
                    .catchOnly[Throwable](readFromString[Result](body))
                    .liftTo[F]
                  err <- Concurrent[F].raiseError[AuthorizationResponse](
                    FailedRequest(
                      response.code.code,
                      res.resultMessage.getOrElse(""),
                      Some(res.toJson)
                    )
                  )
                } yield err // Unreachable, but necessary for type alignment

              case other =>
                logger.error(
                  s"Unhandled HTTP status code $other with body $body"
                ) *> Concurrent[F].raiseError(
                  new Exception(s"Unhandled HTTP status code $other")
                )

            }

          case Left(errorMessage) =>
            Concurrent[F].raiseError(
              new Exception(s"HTTP transport error: $errorMessage")
            )
        }
      }
  }


  override def authAuthorizationFail(
      authorizationFailRequest: AuthorizationFailRequest
  ): F[AuthorizationFailResponse] = ???

  override def authAuthorizationIssue(
      authorizationIssueRequest: AuthorizationIssueRequest
  ): F[AuthorizationIssueResponse] = ???

  override def backchannelAuthentication(
      backchannelAuthenticationRequest: BackchannelAuthenticationRequest
  ): F[BackchannelAuthenticationResponse] = ???

  override def backchannelAuthenticationComplete(
      backchannelAuthenticationCompleteRequest: BackchannelAuthenticationCompleteRequest
  ): F[BackchannelAuthenticationCompleteResponse] = ???

  override def backchannelAuthenticationFail(
      backchannelAuthenticationFailRequest: BackchannelAuthenticationFailRequest
  ): F[BackchannelAuthenticationFailResponse] = ???

  override def backchannelAuthenticationIssue(
      backchannelAuthenticationIssueRequest: BackchannelAuthenticationIssueRequest
  ): F[BackchannelAuthenticationIssueResponse] = ???

  override def clientExtensionRequestablesScopesDelete(
      clientId: String
  ): F[Unit] = ???

  override def clientExtensionRequestablesScopesGet(
      clientId: String
  ): F[ClientExtensionRequestableScopesGetResponse] = ???

  override def clientExtensionRequestablesScopesUpdate(
      clientId: String,
      clientExtensionRequestableScopesUpdateRequest: ClientExtensionRequestableScopesUpdateRequest
  ): F[ClientExtensionRequestableScopesUpdateResponse] = ???

  override def serviceConfiguration(
      pretty: Option[Boolean],
      patch: Option[String]
  ): F[Any] = ???

  override def deviceAuthorization(
      deviceAuthorizationRequest: DeviceAuthorizationRequest
  ): F[DeviceAuthorizationResponse] = ???

  override def deviceComplete(
      deviceCompleteRequest: DeviceCompleteRequest
  ): F[DeviceCompleteResponse] = ???

  override def deviceVerification(
      deviceVerificationRequest: DeviceVerificationRequest
  ): F[DeviceVerificationResponse] = ???

  override def clientRegistration(
      clientRegistrationRequest: ClientRegistrationRequest
  ): F[ClientRegistrationResponse] = ???

  override def clientRegistrationDelete(
      clientRegistrationDeleteRequest: ClientRegistrationDeleteRequest
  ): F[ClientRegistrationDeleteResponse] = ???

  override def clientRegistrationGet(
      clientRegistrationGetRequest: ClientRegistrationGetRequest
  ): F[ClientRegistrationGetResponse] = ???

  override def clientRegistrationUpdate(
      clientRegistrationUpdateRequest: ClientRegistrationUpdateRequest
  ): F[ClientRegistrationUpdateResponse] = ???

  override def grantManagement(gMRequest: GMRequest): F[GMResponse] = ???

  override def hskCreate(
      hskCreateRequest: HskCreateRequest
  ): F[HskCreateResponse] = ???

  override def hskDelete(handle: String): F[HskDeleteResponse] = ???

  override def hskGet(handle: String): F[HskGetResponse] = ???

  override def hskGetList(): F[HskGetListResponse] = ???

  override def idtokenReissue(
      idtokenReissueRequest: Option[IdtokenReissueRequest]
  ): F[IdtokenReissueResponse] = ???

  override def authIntrospection(
      introspectionRequest: IntrospectionRequest
  ): F[IntrospectionResponse] = ???

  override def authIntrospectionStandard(
      standardIntrospectionRequest: StandardIntrospectionRequest
  ): F[StandardIntrospectionResponse] = ???

  override def serviceJwksGet(
      includePrivateKeys: Option[Boolean],
      pretty: Option[Boolean]
  ): F[ServiceJwksGetResponse] = ???

  override def joseVerify(
      joseVerifyRequest: Option[JoseVerifyRequest]
  ): F[JoseVerifyResponse] = ???

  override def pushedAuthReq(
      pushedAuthorizationRequest: PushedAuthorizationRequest
  ): F[PushedAuthorizationResponse] = ???

  override def authRevocation(
      revocationRequest: RevocationRequest
  ): F[RevocationResponse] = ???

  override def info(): F[InfoResponse] = ???

  override def authToken(tokenRequest: TokenRequest): F[TokenResponse] = ???

  override def authTokenFail(
      tokenFailRequest: TokenFailRequest
  ): F[TokenFailResponse] = ???

  override def authTokenIssue(
      tokenIssueRequest: TokenIssueRequest
  ): F[TokenIssueResponse] = ???

  override def authTokenCreate(
      tokenCreateRequest: TokenCreateRequest
  ): F[TokenCreateResponse] = ???

  override def authTokenDelete(accessTokenIdentifier: String): F[Unit] = ???

  override def authTokenGetList(
      clientIdentifier: Option[String],
      subject: Option[String],
      start: Option[Int],
      end: Option[Int]
  ): F[TokenGetListResponse] = ???

  override def authTokenRevoke(
      tokenRevokeRequest: TokenRevokeRequest
  ): F[TokenRevokeResponse] = ???

  override def authTokenUpdate(
      tokenUpdateRequest: TokenUpdateRequest
  ): F[TokenUpdateResponse] = ???

  override def authUserinfo(
      userinfoRequest: UserinfoRequest
  ): F[UserinfoResponse] = ???

  override def authUserinfoIssue(
      userinfoIssueRequest: UserinfoIssueRequest
  ): F[UserinfoIssueResponse] = ???

}
