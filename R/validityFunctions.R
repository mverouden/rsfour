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
#'
#' @export
validAgentObject <- function(object) {
  if (round(sum(object@velocity^2), digits = 1) > 100.0) {
    return("The velocity level is out of bounds.")
  } else {
    return(TRUE)
  }
}
setValidity(Class = "Agent", method = validAgentObject)

#' Check the validity of an object of class Prey.
#'
#' The \code{validPreyObject} function checks whether an object of class
#' \code{\link{Prey}} is a valid object. An object of the \code{\link{Prey-class}}
#' is valid when:
#' \deqn{(object@velocity)^2 \le 70.0}
#' (in this case TRUE is returned), otherwise the message "The velocity is out
#' of bounds." will appear.
#'
#' @param object An object of class \code{\link{Prey}}.
#'
#' @return TRUE or an error message.
#'
#' @seealso
#' \linkS4class{Prey}, \code{\link{Prey-class}},
#'
#' @export
validPreyObject <- function(object) {
  if (round(sum(object@velocity^2), digits = 1) > 70.0) {
    return("The velocity level is out of bounds.")
  } else {
    return(TRUE)
  }
}
setValidity(Class = "Prey", method = validPreyObject)

#' Check the validity of an object of class Bobcat.
#'
#' The \code{validBobcatObject} function checks whether an object of class
#' \code{\link{Bobcat}} is a valid object. An object of the
#' \code{\link{Bobcat-class}} is valid when:
#' \deqn{(object@velocity)^2 \le 85.0}
#' (in this case TRUE is returned), otherwise the message "The velocity is out
#' of bounds." will appear.
#'
#' @param object An object of class \code{\link{Bobcat}}.
#'
#' @return TRUE or an error message.
#'
#' @seealso
#' \linkS4class{Bobcat}, \code{\link{Bobcat-class}},
#'
#' @export
validBobcatObject <- function(object) {
  if (round(sum(object@velocity^2), digits = 1) > 85.0) {
    return("The velocity level is out of bounds.")
  } else {
    return(TRUE)
  }
}
setValidity(Class = "Bobcat", method = validBobcatObject)

#' Check the validity of an object of class Bobcat.
#'
#' The \code{validLynxObject} function checks whether an object of class
#' \code{\link{Lynx}} is a valid object. An object of the \code{\link{Lynx-class}}
#' is valid when:
#' \deqn{(object@velocity)^2 \le 95.0}
#' (in this case TRUE is returned), otherwise the message "The velocity is out
#' of bounds." will appear.
#'
#' @param object An object of class \code{\link{Lynx}}.
#'
#' @return TRUE or an error message.
#'
#' @seealso
#' \linkS4class{Lynx}, \code{\link{Lynx-class}},
#'
#' @export
validLynxObject <- function(object) {
  if (round(sum(object@velocity^2), digits = 1) > 95.0) {
    return("The velocity level is out of bounds.")
  } else {
    return(TRUE)
  }
}
setValidity(Class = "Lynx", method = validLynxObject)
