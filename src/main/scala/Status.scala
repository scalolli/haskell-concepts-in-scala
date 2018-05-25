sealed trait Status

object Status {
  case object Failed extends Status
  case object Success extends Status
}
