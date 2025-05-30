// package authlete.apis
// import authlete.models.{AuthorizationDecisionRequest,AuthorizationDecisionResponse}
// trait AuthorizationDecisionEndpoints[F[_]] {

//   /**
//     * The authorization decision endpoint.
//     *
//     * @see
//     *   <a href="https://datatracker.ietf.org/doc/html/draft-ietf-oauth-dpop-04#section-4.2" >OAuth
//     *   2.1, 4.2. Authorization Decision Endpoint</a>
//     */
//   def authorizationDecision(
//       body: AuthorizationDecisionRequest
//   ): F[AuthorizationDecisionResponse]

//   /**
//     * The authorization decision endpoint for {@code POST} method.
//     *
//     * <p> <a href="https://datatracker.ietf.org/doc/html/draft-ietf-oauth-dpop-04#section-4.2">OAuth
//     * 2.1, 4.2. Authorization Decision Endpoint</a> says: </p>
//     *
//     * <blockquote> <i>The client MUST use the HTTP "POST" method when making authorization decision
//     * requests.</i> </blockquote>
//     */
//   def authorizationDecisionPost(
//       body: AuthorizationDecisionRequest
//   ): F[AuthorizationDecisionResponse]

// }
