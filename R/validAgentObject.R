#' Check the validity of an object of class Agent.
#'
#' The \code{validAgenObject} function checks whether an object of class
#' \code{\link{Agent}} is a valid object. An object of the \code{\link{Agent-class}}
#' is valid when:
#' \deqn{(object@velocity)^2 \le 100.0}
#' (in this case TRUE is returned), otherwise the message "The velocity is out
#' of bounds." will appear.
#'
#' @param object An object of class \code{\link{Agent}}.
#'
#' @return TRUE or an error message.
#'
#' @seealso
#' \linkS4class{Agent}, \code{\link{Agent-class}},
#' \code{\link{initialize,Agent-method}}
#'
#' @examples
#' a <- new("Agent")
#' print(a)
#' a <- initialize(a,
#'                 location = c(1.23, 10.5),
#'                 velocity = c(5 * sqrt(2), 5 * sqrt(2)),
#'                 active = FALSE)
#' validAgentObject(a)
#' print(a)
validAgentObject <- function(object) {
  if (round(sum(object@velocity^2), digits = 1) > 100.0) {
    return("The velocity level is out of bounds.")
  } else {
    return(TRUE)
  }
}
setValidity(Class = "Agent", method = validAgentObject)
