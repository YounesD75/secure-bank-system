package securebank.analytics

case class SecurityEvent(
  eventType: String,   // AUTH_SUCCESS, AUTH_FAILURE, ACCOUNT_LOCKED, TOKEN_REVOKED
  user: String,
  timestamp: Long
)