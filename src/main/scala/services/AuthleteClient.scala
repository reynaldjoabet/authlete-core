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

  def handleHttpFailure(statusCode: Int, body: String) = statusCode match {

    case 400 | 401 | 403 | 500 =>
      for {
        res <- Either
          .catchOnly[Throwable](readFromString[Result](body))
          .liftTo[F]
        err <- Concurrent[F].raiseError[AuthorizationResponse](
          FailedRequest(
            statusCode,
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
  ): F[AuthorizationFailResponse] = {

    val uri = baseUri.withPath(
      List("api", config.apiKey, "auth", "authorization", "fail")
    )

    basicReq
      .post(uri)
      .contentType("application/json")
      .body(authorizationFailRequest.toJson)
      .contentType(MediaType.ApplicationJson)
      .send(backend) // F[Response[Either[String, String]]]
      .flatMap { response =>
        response.body match {
          case Right(body) =>
            response.code.code match {
              case 200 =>
                Either
                  .catchOnly[Throwable](
                    readFromString[AuthorizationFailResponse](body)
                  )
                  .liftTo[F]

              case 400 | 401 | 403 | 500 =>
                for {
                  res <- Either
                    .catchOnly[Throwable](readFromString[Result](body))
                    .liftTo[F]
                  err <- Concurrent[F].raiseError[AuthorizationFailResponse](
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

  override def authAuthorizationIssue(
      authorizationIssueRequest: AuthorizationIssueRequest
  ): F[AuthorizationIssueResponse] = {

    val uri = baseUri.withPath(
      List("api", config.apiKey, "auth", "authorization", "issue")
    )

    basicReq
      .post(uri)
      .contentType("application/json")
      .body(authorizationIssueRequest.toJson)
      .contentType(MediaType.ApplicationJson)
      .send(backend) // F[Response[Either[String, String]]]
      .flatMap { response =>
        response.body match {
          case Right(body) =>
            response.code.code match {
              case 200 =>
                Either
                  .catchOnly[Throwable](
                    readFromString[AuthorizationIssueResponse](body)
                  )
                  .liftTo[F]

              case 400 | 401 | 403 | 500 =>
                for {
                  res <- Either
                    .catchOnly[Throwable](readFromString[Result](body))
                    .liftTo[F]
                  err <- Concurrent[F].raiseError[AuthorizationIssueResponse](
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

  override def backchannelAuthentication(
      backchannelAuthenticationRequest: BackchannelAuthenticationRequest
  ): F[BackchannelAuthenticationResponse] = {

    val uri =
      baseUri.withPath(List("api", config.apiKey, "auth", "backchannel"))

    basicReq
      .post(uri)
      .contentType("application/json")
      .body(backchannelAuthenticationRequest.toJson)
      .contentType(MediaType.ApplicationJson)
      .send(backend) // F[Response[Either[String, String]]]
      .flatMap { response =>
        response.body match {
          case Right(body) =>
            response.code.code match {
              case 200 =>
                Either
                  .catchOnly[Throwable](
                    readFromString[BackchannelAuthenticationResponse](body)
                  )
                  .liftTo[F]

              case 400 | 401 | 403 | 500 =>
                for {
                  res <- Either
                    .catchOnly[Throwable](readFromString[Result](body))
                    .liftTo[F]
                  err <- Concurrent[F]
                    .raiseError[BackchannelAuthenticationResponse](
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

  override def backchannelAuthenticationComplete(
      backchannelAuthenticationCompleteRequest: BackchannelAuthenticationCompleteRequest
  ): F[BackchannelAuthenticationCompleteResponse] = {

    val uri = baseUri.withPath(
      List("api", config.apiKey, "auth", "backchannel", "complete")
    )

    basicReq
      .post(uri)
      .contentType("application/json")
      .body(backchannelAuthenticationCompleteRequest.toJson)
      .contentType(MediaType.ApplicationJson)
      .send(backend) // F[Response[Either[String, String]]]
      .flatMap { response =>
        response.body match {
          case Right(body) =>
            response.code.code match {
              case 200 =>
                Either
                  .catchOnly[Throwable](
                    readFromString[BackchannelAuthenticationCompleteResponse](
                      body
                    )
                  )
                  .liftTo[F]

              case 400 | 401 | 403 | 500 =>
                for {
                  res <- Either
                    .catchOnly[Throwable](readFromString[Result](body))
                    .liftTo[F]
                  err <- Concurrent[F]
                    .raiseError[BackchannelAuthenticationCompleteResponse](
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

  override def backchannelAuthenticationFail(
      backchannelAuthenticationFailRequest: BackchannelAuthenticationFailRequest
  ): F[BackchannelAuthenticationFailResponse] = {

    val uri = baseUri.withPath(
      List("api", config.apiKey, "auth", "backchannel", "fail")
    )

    basicReq
      .post(uri)
      .contentType("application/json")
      .body(backchannelAuthenticationFailRequest.toJson)
      .contentType(MediaType.ApplicationJson)
      .send(backend) // F[Response[Either[String, String]]]
      .flatMap { response =>
        response.body match {
          case Right(body) =>
            response.code.code match {
              case 200 =>
                Either
                  .catchOnly[Throwable](
                    readFromString[BackchannelAuthenticationFailResponse](body)
                  )
                  .liftTo[F]

              case 400 | 401 | 403 | 500 =>
                for {
                  res <- Either
                    .catchOnly[Throwable](readFromString[Result](body))
                    .liftTo[F]
                  err <- Concurrent[F]
                    .raiseError[BackchannelAuthenticationFailResponse](
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

  override def backchannelAuthenticationIssue(
      backchannelAuthenticationIssueRequest: BackchannelAuthenticationIssueRequest
  ): F[BackchannelAuthenticationIssueResponse] = {

    val uri = baseUri.withPath(
      List("api", config.apiKey, "auth", "backchannel", "issue")
    )

    basicReq
      .post(uri)
      .contentType("application/json")
      .body(backchannelAuthenticationIssueRequest.toJson)
      .contentType(MediaType.ApplicationJson)
      .send(backend) // F[Response[Either[String, String]]]
      .flatMap { response =>
        response.body match {
          case Right(body) =>
            response.code.code match {
              case 200 =>
                Either
                  .catchOnly[Throwable](
                    readFromString[BackchannelAuthenticationIssueResponse](body)
                  )
                  .liftTo[F]

              case 400 | 401 | 403 | 500 =>
                for {
                  res <- Either
                    .catchOnly[Throwable](readFromString[Result](body))
                    .liftTo[F]
                  err <- Concurrent[F]
                    .raiseError[BackchannelAuthenticationIssueResponse](
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

  override def clientExtensionRequestablesScopesDelete(
      clientId: String
  ): F[Unit] = {

    val uri = baseUri.withPath(
      List(
        "api",
        config.apiKey,
        "client",
        clientId,
        "extension",
        "requestable-scopes"
      )
    )

    basicReq
      .delete(uri)
      .contentType(MediaType.ApplicationJson)
      .send(backend) // F[Response[Either[String, String]]]
      .flatMap { response =>
        response.body match {
          case Right(_) if response.code == StatusCode.NoContent =>
            Concurrent[F].unit

          case Right(body) =>
            Concurrent[F].raiseError(
              new Exception(s"Unexpected response body: $body")
            )

          case Left(errorMessage) =>
            Concurrent[F].raiseError(
              new Exception(s"HTTP transport error: $errorMessage")
            )
        }
      }

  }

  override def clientExtensionRequestablesScopesGet(
      clientId: String
  ): F[ClientExtensionRequestableScopesGetResponse] = {

    val uri = baseUri.withPath(
      List(
        "api",
        config.apiKey,
        "client",
        clientId,
        "extension",
        "requestable-scopes"
      )
    )

    basicReq
      .get(uri)
      // .contentType(MediaType.ApplicationJson)
      .send(backend) // F[Response[Either[String, String]]]
      .flatMap { response =>
        response.body match {
          case Right(body) =>
            response.code.code match {
              case 200 =>
                Either
                  .catchOnly[Throwable](
                    readFromString[ClientExtensionRequestableScopesGetResponse](
                      body
                    )
                  )
                  .liftTo[F]

              case 204 =>
                Concurrent[F].pure(
                  ClientExtensionRequestableScopesGetResponse(None)
                )

              case 400 | 401 | 403 | 500 =>
                for {
                  res <- Either
                    .catchOnly[Throwable](readFromString[Result](body))
                    .liftTo[F]
                  err <- Concurrent[F]
                    .raiseError[ClientExtensionRequestableScopesGetResponse](
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

  override def clientExtensionRequestablesScopesUpdate(
      clientId: String,
      clientExtensionRequestableScopesUpdateRequest: ClientExtensionRequestableScopesUpdateRequest
  ): F[ClientExtensionRequestableScopesUpdateResponse] = {

    val uri = baseUri.withPath(
      List(
        "api",
        config.apiKey,
        "client",
        clientId,
        "extension",
        "requestable-scopes"
      )
    )

    basicReq
      .put(uri)
      .contentType("application/json")
      .body(clientExtensionRequestableScopesUpdateRequest.toJson)
      .contentType(MediaType.ApplicationJson)
      .send(backend) // F[Response[Either[String, String]]]
      .flatMap { response =>
        response.body match {
          case Right(body) =>
            response.code.code match {
              case 200 =>
                Either
                  .catchOnly[Throwable](
                    readFromString[
                      ClientExtensionRequestableScopesUpdateResponse
                    ](body)
                  )
                  .liftTo[F]

              case 400 | 401 | 403 | 500 =>
                for {
                  res <- Either
                    .catchOnly[Throwable](readFromString[Result](body))
                    .liftTo[F]
                  err <- Concurrent[F]
                    .raiseError[ClientExtensionRequestableScopesUpdateResponse](
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

  override def serviceConfiguration(
      pretty: Option[Boolean],
      patch: Option[String]
  ): F[String] = {

    val uri = baseUri
      .withPath(List("api", config.apiKey, "service", "configuration"))
      .addParams(
        Map(
          "pretty" -> pretty.map(_.toString),
          "patch" -> patch
        ).collect { case (k, Some(v)) => k -> v }
      )

    basicReq
      .get(uri)
      .contentType(MediaType.ApplicationJson)
      .send(backend) // F[Response[Either[String, String]]]
      .flatMap { response =>
        response.body match {
          case Right(body) =>
            response.code.code match {
              case 200 => body.pure[F]
              case 400 | 401 | 403 | 500 =>
                for {
                  res <- Either
                    .catchOnly[Throwable](readFromString[Result](body))
                    .liftTo[F]
                  err <- Concurrent[F].raiseError[String](
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

  override def deviceAuthorization(
      deviceAuthorizationRequest: DeviceAuthorizationRequest
  ): F[DeviceAuthorizationResponse] = {
    val uri = baseUri.withPath(List("api", config.apiKey, "auth", "device"))
    basicReq
      .post(uri)
      .contentType("application/json")
      .body(deviceAuthorizationRequest.toJson)
      .contentType(MediaType.ApplicationJson)
      .send(backend) // F[Response[Either[String, String]]]
      .flatMap { response =>
        response.body match {
          case Right(body) =>
            response.code.code match {
              case 200 =>
                Either
                  .catchOnly[Throwable](
                    readFromString[DeviceAuthorizationResponse](body)
                  )
                  .liftTo[F]

              case 400 | 401 | 403 | 500 =>
                for {
                  res <- Either
                    .catchOnly[Throwable](readFromString[Result](body))
                    .liftTo[F]
                  err <- Concurrent[F].raiseError[DeviceAuthorizationResponse](
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

  override def deviceComplete(
      deviceCompleteRequest: DeviceCompleteRequest
  ): F[DeviceCompleteResponse] = {

    val uri =
      baseUri.withPath(List("api", config.apiKey, "auth", "device", "complete"))

    basicReq
      .post(uri)
      .contentType("application/json")
      .body(deviceCompleteRequest.toJson)
      .contentType(MediaType.ApplicationJson)
      .send(backend) // F[Response[Either[String, String]]]
      .flatMap { response =>
        response.body match {
          case Right(body) =>
            response.code.code match {
              case 200 =>
                Either
                  .catchOnly[Throwable](
                    readFromString[DeviceCompleteResponse](body)
                  )
                  .liftTo[F]

              case 400 | 401 | 403 | 500 =>
                for {
                  res <- Either
                    .catchOnly[Throwable](readFromString[Result](body))
                    .liftTo[F]
                  err <- Concurrent[F].raiseError[DeviceCompleteResponse](
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

  override def deviceVerification(
      deviceVerificationRequest: DeviceVerificationRequest
  ): F[DeviceVerificationResponse] = {

    val uri = baseUri.withPath(
      List("api", config.apiKey, "auth", "device", "verification")
    )

    basicReq
      .post(uri)
      .contentType("application/json")
      .body(deviceVerificationRequest.toJson)
      .contentType(MediaType.ApplicationJson)
      .send(backend) // F[Response[Either[String, String]]]
      .flatMap { response =>
        response.body match {
          case Right(body) =>
            response.code.code match {
              case 200 =>
                Either
                  .catchOnly[Throwable](
                    readFromString[DeviceVerificationResponse](body)
                  )
                  .liftTo[F]

              case 400 | 401 | 403 | 500 =>
                for {
                  res <- Either
                    .catchOnly[Throwable](readFromString[Result](body))
                    .liftTo[F]
                  err <- Concurrent[F].raiseError[DeviceVerificationResponse](
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

  override def clientRegistration(
      clientRegistrationRequest: ClientRegistrationRequest
  ): F[ClientRegistrationResponse] = {
    val uri =
      baseUri.withPath(List("api", config.apiKey, "client", "registration"))

    basicReq
      .post(uri)
      .contentType("application/json")
      .body(clientRegistrationRequest.toJson)
      .contentType(MediaType.ApplicationJson)
      .send(backend) // F[Response[Either[String, String]]]
      .flatMap { response =>
        response.body match {
          case Right(body) =>
            response.code.code match {
              case 200 =>
                Either
                  .catchOnly[Throwable](
                    readFromString[ClientRegistrationResponse](body)
                  )
                  .liftTo[F]

              case 400 | 401 | 403 | 500 =>
                for {
                  res <- Either
                    .catchOnly[Throwable](readFromString[Result](body))
                    .liftTo[F]
                  err <- Concurrent[F].raiseError[ClientRegistrationResponse](
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

  override def clientRegistrationDelete(
      clientRegistrationDeleteRequest: ClientRegistrationDeleteRequest
  ): F[ClientRegistrationDeleteResponse] = {
    val uri = baseUri.withPath(
      List(
        "api",
        config.apiKey,
        "client",
        "registration",
        clientRegistrationDeleteRequest.clientId
      )
    )

    basicReq
      .delete(uri)
      .contentType(MediaType.ApplicationJson)
      .send(backend) // F[Response[Either[String, String]]]
      .flatMap { response =>
        response.body match {
          case Right(body) =>
            response.code.code match {
              case 200 =>
                Either
                  .catchOnly[Throwable](
                    readFromString[ClientRegistrationDeleteResponse](body)
                  )
                  .liftTo[F]

              case 400 | 401 | 403 | 500 =>
                for {
                  res <- Either
                    .catchOnly[Throwable](readFromString[Result](body))
                    .liftTo[F]
                  err <- Concurrent[F]
                    .raiseError[ClientRegistrationDeleteResponse](
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

  override def clientRegistrationGet(
      clientRegistrationGetRequest: ClientRegistrationGetRequest
  ): F[ClientRegistrationGetResponse] = {

    val uri = baseUri.withPath(
      List(
        "api",
        config.apiKey,
        "client",
        "registration",
        clientRegistrationGetRequest.clientId
      )
    )

    basicReq
      .get(uri)
      .contentType(MediaType.ApplicationJson)
      .send(backend) // F[Response[Either[String, String]]]
      .flatMap { response =>
        response.body match {
          case Right(body) =>
            response.code.code match {
              case 200 =>
                Either
                  .catchOnly[Throwable](
                    readFromString[ClientRegistrationGetResponse](body)
                  )
                  .liftTo[F]

              case 400 | 401 | 403 | 500 =>
                for {
                  res <- Either
                    .catchOnly[Throwable](readFromString[Result](body))
                    .liftTo[F]
                  err <- Concurrent[F]
                    .raiseError[ClientRegistrationGetResponse](
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

  override def clientRegistrationUpdate(
      clientRegistrationUpdateRequest: ClientRegistrationUpdateRequest
  ): F[ClientRegistrationUpdateResponse] = {

    val uri = baseUri.withPath(
      List(
        "api",
        config.apiKey,
        "client",
        "registration",
        clientRegistrationUpdateRequest.clientId
      )
    )

    basicReq
      .put(uri)
      .contentType("application/json")
      .body(clientRegistrationUpdateRequest.toJson)
      .contentType(MediaType.ApplicationJson)
      .send(backend) // F[Response[Either[String, String]]]
      .flatMap { response =>
        response.body match {
          case Right(body) =>
            response.code.code match {
              case 200 =>
                Either
                  .catchOnly[Throwable](
                    readFromString[ClientRegistrationUpdateResponse](body)
                  )
                  .liftTo[F]

              case 400 | 401 | 403 | 500 =>
                for {
                  res <- Either
                    .catchOnly[Throwable](readFromString[Result](body))
                    .liftTo[F]
                  err <- Concurrent[F]
                    .raiseError[ClientRegistrationUpdateResponse](
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

  override def grantManagement(gMRequest: GMRequest): F[GMResponse] = {
    val uri =
      baseUri.withPath(List("api", config.apiKey, "grant", "management"))

    basicReq
      .post(uri)
      .contentType("application/json")
      .body(gMRequest.toJson)
      .contentType(MediaType.ApplicationJson)
      .send(backend) // F[Response[Either[String, String]]]
      .flatMap { response =>
        response.body match {
          case Right(body) =>
            response.code.code match {
              case 200 =>
                Either
                  .catchOnly[Throwable](readFromString[GMResponse](body))
                  .liftTo[F]

              case 400 | 401 | 403 | 500 =>
                for {
                  res <- Either
                    .catchOnly[Throwable](readFromString[Result](body))
                    .liftTo[F]
                  err <- Concurrent[F].raiseError[GMResponse](
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

  override def hskCreate(
      hskCreateRequest: HskCreateRequest
  ): F[HskCreateResponse] = {

    val uri = baseUri.withPath(List("api", config.apiKey, "hsk", "create"))

    basicReq
      .post(uri)
      .contentType("application/json")
      .body(hskCreateRequest.toJson)
      .contentType(MediaType.ApplicationJson)
      .send(backend) // F[Response[Either[String, String]]]
      .flatMap { response =>
        response.body match {
          case Right(body) =>
            response.code.code match {
              case 200 =>
                Either
                  .catchOnly[Throwable](
                    readFromString[HskCreateResponse](body)
                  )
                  .liftTo[F]

              case 400 | 401 | 403 | 500 =>
                for {
                  res <- Either
                    .catchOnly[Throwable](readFromString[Result](body))
                    .liftTo[F]
                  err <- Concurrent[F].raiseError[HskCreateResponse](
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

  override def hskDelete(handle: String): F[HskDeleteResponse] = {
    val uri =
      baseUri.withPath(List("api", config.apiKey, "hsk", handle, "delete"))

    basicReq
      .delete(uri)
      .contentType(MediaType.ApplicationJson)
      .send(backend) // F[Response[Either[String, String]]]
      .flatMap { response =>
        response.body match {
          case Right(body) =>
            response.code.code match {
              case 200 =>
                Either
                  .catchOnly[Throwable](
                    readFromString[HskDeleteResponse](body)
                  )
                  .liftTo[F]

              case 400 | 401 | 403 | 500 =>
                for {
                  res <- Either
                    .catchOnly[Throwable](readFromString[Result](body))
                    .liftTo[F]
                  err <- Concurrent[F].raiseError[HskDeleteResponse](
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

  override def hskGet(handle: String): F[HskGetResponse] = {
    val uri = baseUri.withPath(List("api", config.apiKey, "hsk", handle))

    basicReq
      .get(uri)
      .contentType(MediaType.ApplicationJson)
      .send(backend) // F[Response[Either[String, String]]]
      .flatMap { response =>
        response.body match {
          case Right(body) =>
            response.code.code match {
              case 200 =>
                Either
                  .catchOnly[Throwable](
                    readFromString[HskGetResponse](body)
                  )
                  .liftTo[F]

              case 400 | 401 | 403 | 500 =>
                for {
                  res <- Either
                    .catchOnly[Throwable](readFromString[Result](body))
                    .liftTo[F]
                  err <- Concurrent[F].raiseError[HskGetResponse](
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

  override def hskGetList(): F[HskGetListResponse] = {

    val uri = baseUri.withPath(List("api", config.apiKey, "hsk", "list"))

    basicReq
      .get(uri)
      .contentType(MediaType.ApplicationJson)
      .send(backend) // F[Response[Either[String, String]]]
      .flatMap { response =>
        response.body match {
          case Right(body) =>
            response.code.code match {
              case 200 =>
                Either
                  .catchOnly[Throwable](
                    readFromString[HskGetListResponse](body)
                  )
                  .liftTo[F]

              case 400 | 401 | 403 | 500 =>
                for {
                  res <- Either
                    .catchOnly[Throwable](readFromString[Result](body))
                    .liftTo[F]
                  err <- Concurrent[F].raiseError[HskGetListResponse](
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

  override def idtokenReissue(
      idtokenReissueRequest: Option[IdtokenReissueRequest]
  ): F[IdtokenReissueResponse] = {
    val uri = baseUri.withPath(List("api", config.apiKey, "idtoken", "reissue"))

    basicReq
      .post(uri)
      .contentType("application/json")
      .body(idtokenReissueRequest.toJson)
      .contentType(MediaType.ApplicationJson)
      .send(backend) // F[Response[Either[String, String]]]
      .flatMap { response =>
        response.body match {
          case Right(body) =>
            response.code.code match {
              case 200 =>
                Either
                  .catchOnly[Throwable](
                    readFromString[IdtokenReissueResponse](body)
                  )
                  .liftTo[F]

              case 400 | 401 | 403 | 500 =>
                for {
                  res <- Either
                    .catchOnly[Throwable](readFromString[Result](body))
                    .liftTo[F]
                  err <- Concurrent[F].raiseError[IdtokenReissueResponse](
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

  override def authIntrospection(
      introspectionRequest: IntrospectionRequest
  ): F[IntrospectionResponse] = {
    val uri =
      baseUri.withPath(List("api", config.apiKey, "auth", "introspection"))

    basicReq
      .post(uri)
      .contentType("application/json")
      .body(introspectionRequest.toJson)
      .contentType(MediaType.ApplicationJson)
      .send(backend) // F[Response[Either[String, String]]]
      .flatMap { response =>
        response.body match {
          case Right(body) =>
            response.code.code match {
              case 200 =>
                Either
                  .catchOnly[Throwable](
                    readFromString[IntrospectionResponse](body)
                  )
                  .liftTo[F]

              case 400 | 401 | 403 | 500 =>
                for {
                  res <- Either
                    .catchOnly[Throwable](readFromString[Result](body))
                    .liftTo[F]
                  err <- Concurrent[F].raiseError[IntrospectionResponse](
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

  override def authIntrospectionStandard(
      standardIntrospectionRequest: StandardIntrospectionRequest
  ): F[StandardIntrospectionResponse] = {
    val uri = baseUri.withPath(
      List("api", config.apiKey, "auth", "introspection", "standard")
    )

    basicReq
      .post(uri)
      .contentType("application/json")
      .body(standardIntrospectionRequest.toJson)
      .contentType(MediaType.ApplicationJson)
      .send(backend) // F[Response[Either[String, String]]]
      .flatMap { response =>
        response.body match {
          case Right(body) =>
            response.code.code match {
              case 200 =>
                Either
                  .catchOnly[Throwable](
                    readFromString[StandardIntrospectionResponse](body)
                  )
                  .liftTo[F]

              case 400 | 401 | 403 | 500 =>
                for {
                  res <- Either
                    .catchOnly[Throwable](readFromString[Result](body))
                    .liftTo[F]
                  err <- Concurrent[F]
                    .raiseError[StandardIntrospectionResponse](
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

  override def serviceJwksGet(
      includePrivateKeys: Option[Boolean],
      pretty: Option[Boolean]
  ): F[ServiceJwksGetResponse] = {

    val uri = baseUri.withPath(List("api", config.apiKey, "service", "jwks"))
    basicReq
      .get(uri)
      .contentType(MediaType.ApplicationJson)
      .send(backend) // F[Response[Either[String, String]]]
      .flatMap { response =>
        response.body match {
          case Right(body) =>
            response.code.code match {
              case 200 =>
                Either
                  .catchOnly[Throwable](
                    readFromString[ServiceJwksGetResponse](body)
                  )
                  .liftTo[F]

              case 204 =>
                Concurrent[F].pure(ServiceJwksGetResponse(None))

              case 400 | 401 | 403 | 500 =>
                for {
                  res <- Either
                    .catchOnly[Throwable](readFromString[Result](body))
                    .liftTo[F]
                  err <- Concurrent[F].raiseError[ServiceJwksGetResponse](
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

  override def joseVerify(
      joseVerifyRequest: Option[JoseVerifyRequest]
  ): F[JoseVerifyResponse] = {

    val uri = baseUri.withPath(List("api", config.apiKey, "jose", "verify"))

    basicReq
      .post(uri)
      .contentType("application/json")
      .body(joseVerifyRequest.toJson)
      .contentType(MediaType.ApplicationJson)
      .send(backend) // F[Response[Either[String, String]]]
      .flatMap { response =>
        response.body match {
          case Right(body) =>
            response.code.code match {
              case 200 =>
                Either
                  .catchOnly[Throwable](
                    readFromString[JoseVerifyResponse](body)
                  )
                  .liftTo[F]

              case 400 | 401 | 403 | 500 =>
                for {
                  res <- Either
                    .catchOnly[Throwable](readFromString[Result](body))
                    .liftTo[F]
                  err <- Concurrent[F].raiseError[JoseVerifyResponse](
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

  override def pushedAuthReq(
      pushedAuthorizationRequest: PushedAuthorizationRequest
  ): F[PushedAuthorizationResponse] = {
    val uri = baseUri.withPath(List("api", config.apiKey, "auth", "pushed"))

    basicReq
      .post(uri)
      .contentType("application/json")
      .body(pushedAuthorizationRequest.toJson)
      .contentType(MediaType.ApplicationJson)
      .send(backend) // F[Response[Either[String, String]]]
      .flatMap { response =>
        response.body match {
          case Right(body) =>
            response.code.code match {
              case 200 =>
                Either
                  .catchOnly[Throwable](
                    readFromString[PushedAuthorizationResponse](body)
                  )
                  .liftTo[F]

              case 400 | 401 | 403 | 500 =>
                for {
                  res <- Either
                    .catchOnly[Throwable](readFromString[Result](body))
                    .liftTo[F]
                  err <- Concurrent[F].raiseError[PushedAuthorizationResponse](
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

  override def authRevocation(
      revocationRequest: RevocationRequest
  ): F[RevocationResponse] = {
    val uri = baseUri.withPath(List("api", config.apiKey, "auth", "revocation"))

    basicReq
      .post(uri)
      .contentType("application/json")
      .body(revocationRequest.toJson)
      .contentType(MediaType.ApplicationJson)
      .send(backend) // F[Response[Either[String, String]]]
      .flatMap { response =>
        response.body match {
          case Right(body) =>
            response.code.code match {
              case 200 =>
                Either
                  .catchOnly[Throwable](
                    readFromString[RevocationResponse](body)
                  )
                  .liftTo[F]

              case 400 | 401 | 403 | 500 =>
                for {
                  res <- Either
                    .catchOnly[Throwable](readFromString[Result](body))
                    .liftTo[F]
                  err <- Concurrent[F].raiseError[RevocationResponse](
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

  override def info(): F[InfoResponse] = {

    val uri = baseUri.withPath(List("api", config.apiKey, "info"))

    basicReq
      .get(uri)
      .contentType(MediaType.ApplicationJson)
      .send(backend) // F[Response[Either[String, String]]]
      .flatMap { response =>
        response.body match {
          case Right(body) =>
            response.code.code match {
              case 200 =>
                Either
                  .catchOnly[Throwable](readFromString[InfoResponse](body))
                  .liftTo[F]

              case 400 | 401 | 403 | 500 =>
                for {
                  res <- Either
                    .catchOnly[Throwable](readFromString[Result](body))
                    .liftTo[F]
                  err <- Concurrent[F].raiseError[InfoResponse](
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

  override def authToken(tokenRequest: TokenRequest): F[TokenResponse] = {

    val uri = baseUri.withPath(List("api", config.apiKey, "auth", "token"))
    basicReq
      .post(uri)
      .contentType("application/json")
      .body(tokenRequest.toJson)
      .contentType(MediaType.ApplicationJson)
      .send(backend) // F[Response[Either[String, String]]]
      .flatMap { response =>
        response.body match {
          case Right(body) =>
            response.code.code match {
              case 200 =>
                Either
                  .catchOnly[Throwable](readFromString[TokenResponse](body))
                  .liftTo[F]

              case 400 | 401 | 403 | 500 =>
                for {
                  res <- Either
                    .catchOnly[Throwable](readFromString[Result](body))
                    .liftTo[F]
                  err <- Concurrent[F].raiseError[TokenResponse](
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

  override def authTokenFail(
      tokenFailRequest: TokenFailRequest
  ): F[TokenFailResponse] = {

    val uri =
      baseUri.withPath(List("api", config.apiKey, "auth", "token", "fail"))

    basicReq
      .post(uri)
      .contentType("application/json")
      .body(tokenFailRequest.toJson)
      .contentType(MediaType.ApplicationJson)
      .send(backend) // F[Response[Either[String, String]]]
      .flatMap { response =>
        response.body match {
          case Right(body) =>
            response.code.code match {
              case 200 =>
                Either
                  .catchOnly[Throwable](
                    readFromString[TokenFailResponse](body)
                  )
                  .liftTo[F]

              case 400 | 401 | 403 | 500 =>
                for {
                  res <- Either
                    .catchOnly[Throwable](readFromString[Result](body))
                    .liftTo[F]
                  err <- Concurrent[F].raiseError[TokenFailResponse](
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

  override def authTokenIssue(
      tokenIssueRequest: TokenIssueRequest
  ): F[TokenIssueResponse] = {

    val uri =
      baseUri.withPath(List("api", config.apiKey, "auth", "token", "issue"))

    basicReq
      .post(uri)
      .contentType("application/json")
      .body(tokenIssueRequest.toJson)
      .contentType(MediaType.ApplicationJson)
      .send(backend) // F[Response[Either[String, String]]]
      .flatMap { response =>
        response.body match {
          case Right(body) =>
            response.code.code match {
              case 200 =>
                Either
                  .catchOnly[Throwable](
                    readFromString[TokenIssueResponse](body)
                  )
                  .liftTo[F]

              case 400 | 401 | 403 | 500 =>
                for {
                  res <- Either
                    .catchOnly[Throwable](readFromString[Result](body))
                    .liftTo[F]
                  err <- Concurrent[F].raiseError[TokenIssueResponse](
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

  override def authTokenCreate(
      tokenCreateRequest: TokenCreateRequest
  ): F[TokenCreateResponse] = {

    val uri =
      baseUri.withPath(List("api", config.apiKey, "auth", "token", "create"))

    basicReq
      .post(uri)
      .contentType("application/json")
      .body(tokenCreateRequest.toJson)
      .contentType(MediaType.ApplicationJson)
      .send(backend) // F[Response[Either[String, String]]]
      .flatMap { response =>
        response.body match {
          case Right(body) =>
            response.code.code match {
              case 200 =>
                Either
                  .catchOnly[Throwable](
                    readFromString[TokenCreateResponse](body)
                  )
                  .liftTo[F]

              case 400 | 401 | 403 | 500 =>
                for {
                  res <- Either
                    .catchOnly[Throwable](readFromString[Result](body))
                    .liftTo[F]
                  err <- Concurrent[F].raiseError[TokenCreateResponse](
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

  override def authTokenDelete(accessTokenIdentifier: String): F[Unit] = {
    val uri = baseUri.withPath(
      List("api", config.apiKey, "auth", "token", accessTokenIdentifier)
    )

    basicReq
      .delete(uri)
      .contentType(MediaType.ApplicationJson)
      .send(backend) // F[Response[Either[String, String]]]
      .flatMap { response =>
        response.body match {
          case Right(body) =>
            response.code.code match {
              case 204 => Concurrent[F].unit // No content, successful deletion

              case 400 | 401 | 403 | 500 =>
                for {
                  res <- Either
                    .catchOnly[Throwable](readFromString[Result](body))
                    .liftTo[F]
                  err <- Concurrent[F].raiseError[Unit](
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

  override def authTokenGetList(
      clientIdentifier: Option[String],
      subject: Option[String],
      start: Option[Int],
      end: Option[Int]
  ): F[TokenGetListResponse] = {

    val uri = baseUri
      .withPath(List("api", config.apiKey, "auth", "token", "list"))
      .addParams(
        Map(
          "clientIdentifier" -> clientIdentifier,
          "subject" -> subject,
          "start" -> start.map(_.toString),
          "end" -> end.map(_.toString)
        ).collect { case (k, Some(v)) => k -> v }
      )

    basicReq
      .get(uri)
      .contentType(MediaType.ApplicationJson)
      .send(backend) // F[Response[Either[String, String]]]
      .flatMap { response =>
        response.body match {
          case Right(body) =>
            response.code.code match {
              case 200 =>
                Either
                  .catchOnly[Throwable](
                    readFromString[TokenGetListResponse](body)
                  )
                  .liftTo[F]

              case 400 | 401 | 403 | 500 =>
                for {
                  res <- Either
                    .catchOnly[Throwable](readFromString[Result](body))
                    .liftTo[F]
                  err <- Concurrent[F].raiseError[TokenGetListResponse](
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

  override def authTokenRevoke(
      tokenRevokeRequest: TokenRevokeRequest
  ): F[TokenRevokeResponse] = {
    val uri =
      baseUri.withPath(List("api", config.apiKey, "auth", "token", "revoke"))

    basicReq
      .post(uri)
      .contentType("application/json")
      .body(tokenRevokeRequest.toJson)
      .contentType(MediaType.ApplicationJson)
      .send(backend) // F[Response[Either[String, String]]]
      .flatMap { response =>
        response.body match {
          case Right(body) =>
            response.code.code match {
              case 200 =>
                Either
                  .catchOnly[Throwable](
                    readFromString[TokenRevokeResponse](body)
                  )
                  .liftTo[F]

              case 400 | 401 | 403 | 500 =>
                for {
                  res <- Either
                    .catchOnly[Throwable](readFromString[Result](body))
                    .liftTo[F]
                  err <- Concurrent[F].raiseError[TokenRevokeResponse](
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

  override def authTokenUpdate(
      tokenUpdateRequest: TokenUpdateRequest
  ): F[TokenUpdateResponse] = {

    val uri =
      baseUri.withPath(List("api", config.apiKey, "auth", "token", "update"))

    basicReq
      .post(uri)
      .contentType("application/json")
      .body(tokenUpdateRequest.toJson)
      .contentType(MediaType.ApplicationJson)
      .send(backend) // F[Response[Either[String, String]]]
      .flatMap { response =>
        response.body match {
          case Right(body) =>
            response.code.code match {
              case 200 =>
                Either
                  .catchOnly[Throwable](
                    readFromString[TokenUpdateResponse](body)
                  )
                  .liftTo[F]

              case 400 | 401 | 403 | 500 =>
                for {
                  res <- Either
                    .catchOnly[Throwable](readFromString[Result](body))
                    .liftTo[F]
                  err <- Concurrent[F].raiseError[TokenUpdateResponse](
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

  override def authUserinfo(
      userinfoRequest: UserinfoRequest
  ): F[UserinfoResponse] = {

    val uri = baseUri.withPath(List("api", config.apiKey, "auth", "userinfo"))

    basicReq
      .post(uri)
      .contentType("application/json")
      .body(userinfoRequest.toJson)
      .contentType(MediaType.ApplicationJson)
      .send(backend) // F[Response[Either[String, String]]]
      .flatMap { response =>
        response.body match {
          case Right(body) =>
            response.code.code match {
              case 200 =>
                Either
                  .catchOnly[Throwable](readFromString[UserinfoResponse](body))
                  .liftTo[F]

              case 400 | 401 | 403 | 500 =>
                for {
                  res <- Either
                    .catchOnly[Throwable](readFromString[Result](body))
                    .liftTo[F]
                  err <- Concurrent[F].raiseError[UserinfoResponse](
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

  override def authUserinfoIssue(
      userinfoIssueRequest: UserinfoIssueRequest
  ): F[UserinfoIssueResponse] = {

    val uri =
      baseUri.withPath(List("api", config.apiKey, "auth", "userinfo", "issue"))

    basicReq
      .post(uri)
      .contentType("application/json")
      .body(userinfoIssueRequest.toJson)
      .contentType(MediaType.ApplicationJson)
      .send(backend) // F[Response[Either[String, String]]]
      .flatMap { response =>
        response.body match {
          case Right(body) =>
            response.code.code match {
              case 200 =>
                Either
                  .catchOnly[Throwable](
                    readFromString[UserinfoIssueResponse](body)
                  )
                  .liftTo[F]

              case 400 | 401 | 403 | 500 =>
                for {
                  res <- Either
                    .catchOnly[Throwable](readFromString[Result](body))
                    .liftTo[F]
                  err <- Concurrent[F].raiseError[UserinfoIssueResponse](
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

}
