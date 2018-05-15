### Define Classes ----
#' An S4 class to represent an agent.
#'
#' @slot location A numeric vector for specifying the object's location, where
#'                the first value specifies x and the second y  [Default:
#'                c(0.0, 0.0)].
#' @slot velocity A numeric vector for specifying the object's velocity, where
#'                the first value specifies the velocity in the x-direction and
#'                the second in the y-direction [Default: c(0.0, 0.0)].
#' @slot active A logical vector indicating whether th object is active
#'              [default: TRUE].
#'
#' @seealso \code{\link{validAgentObject}}, \code{\link{initialize,Agent-method}}
#'
#' @examples
#' a <- new("Agent")
#' print(a)
#' a <- initialize(a,
#'                 location = c(1.23, 10.5),
#'                 velocity = c(5 * sqrt(2), 5 * sqrt(2)),
#'                 active = TRUE)
#' validAgentObject(a)
#' print(a)
#'
#' @export
#' @import methods
Agent <- setClass(
  ## Set the name for the class
  Class = "Agent",
  ## Define the slots
  slots = c(
    location = "numeric",
    velocity = "numeric",
    active = "logical"
  )
)

### Initialize Classes ----
#' Initialize an object of class "Agent".
#'
#' The initialize function sets the values for the slots of an object of class
#' \linkS4class{Agent}.
#'
#' @param .Object Name of an object of class \linkS4class{Agent}.
#' @param location A numerical vector of length 2 specifying the x- and
#'                 y-position of the object of class \linkS4class{Agent}. If not
#'                 specified the default location will be c(0, 0).
#' @param velocity A numerical vector of length 2 specifying the velocity in the
#'                 x- and y-direction of the object of class
#'                 \linkS4class{Agent}. If not specified the default velocity
#'                 will be c(0, 0).
#' @param active A length-one logical vector indicating whether the object of
#'               class \linkS4class{Agent} is active (value: TRUE) or not
#'               (value: FALSE). If not specified the default value will be TRUE.
#'
#' @seealso
#' \linkS4class{Agent}, \code{\link{Agent-class}},
#' \code{\link{validAgentObject}}
#'
#' @examples
#' a <- new("Agent")
#' print(a)
#' a <- initialize(a,
#'                 location = c(1.23, 10.5),
#'                 velocity = c(5 * sqrt(2), 5 * sqrt(2)),
#'                 active = TRUE)
#' validAgentObject(a)
#' print(a)
#'
#' @export
setMethod(f = "initialize",
          signature = "Agent",
          definition = function(.Object, location = numeric(0), velocity = numeric(0), active = logical(0)) {
            if (length(location) == 0) {
              .Object@location <- c(0.0, 0.0)
            } else if (length(location) < 2 | length(location) > 2) {
              stop("The loction argument should have two values.")
            } else {
              .Object@location <- location
            }
            if (length(velocity) == 0) {
              .Object@velocity <- c(0.0, 0.0)
            } else if (length(velocity) < 2 | length(velocity) > 2) {
              stop("The velocity argument should have two values.")
            } else {
              .Object@velocity <- velocity
            }
            if (length(active) == 0) {
              .Object@active <- TRUE
            } else if (length(active) > 1) {
              stop("The active argument can only be a length-one logical vector (TRUE or FALSE).")
            } else {
              .Object@active <- active
            }
            if (validAgentObject(.Object) == TRUE) {
              return(.Object)
            } else {
              stop("The velocity level is out of bounds.")
            }
          })

### Validity Checks ----
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
#' @return TRUE or an error message
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
#'                 active = TRUE)
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
