package authlete
package apis

object BaseClient {
// private val basicReq = basicRequest
//   .auth
//   // .basic(config.apiKey, config.apiSecret)// V3 API requires an access token, not a key and secret
//   .bearer(config.auth)
//   .contentType(MediaType.ApplicationJson)
//   .readTimeout(config.requestTimeout)

// private val baseUri = Uri(config.baseUrl)

// private[services] def postRequest[B: JsonValueCodec, R: JsonValueCodec](uri: Uri, body: B) =
//   basicReq.post(uri).body(body.toJson).contentType(MediaType.ApplicationJson).response(asJson[R])

// private[services] def postRequestWithUnitResponse[B: JsonValueCodec](uri: Uri, body: B) =
//   basicReq.post(uri).body(body.toJson).contentType(MediaType.ApplicationJson).response(ignore)

// private[services] def postRequest[R: JsonValueCodec](uri: Uri) =
//   basicReq.post(uri).response(asJson[R])

// private[services] def postRequestWithUnitResponse(uri: Uri) =
//   basicReq.post(uri).response(ignore)

// private[services] def putRequest[B: JsonValueCodec, R: JsonValueCodec](uri: Uri, body: B) =
//   basicReq.put(uri).body(body.toJson).contentType(MediaType.ApplicationJson).response(asJson[R])

// // If you set the body as a Map[String, String] or Seq[(String, String)], it will be encoded as form-data (as if a web form with the given values was submitted)
// private[services] def postRequestForm[R: JsonValueCodec](uri: Uri, body: Map[String, String]) =
//   basicReq
//     .post(uri)
//     .body(body)
//     .response(asJson[R])
//     .contentType(MediaType.ApplicationXWwwFormUrlencoded)

// private[services] def getRequest[R: JsonValueCodec](uri: Uri) =
//   basicReq.get(uri).response(asJson[R])

// private[services] def deleteRequestWithUnitResponse(uri: Uri) =
//   basicReq.delete(uri).response(ignore)

// private[services] def deleteRequest[R: JsonValueCodec](uri: Uri) =
//   basicReq.delete(uri).response(asJson[R])

}
