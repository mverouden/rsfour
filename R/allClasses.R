### Define Classes ----
#' An S4 class to represent an agent.
#'
#' @slot location A numerical vector of length 2 specifying the x- and
#'                y-position of the object of class \linkS4class{Agent}
#'               [Default: c(0, 0)].
#' @slot velocity A numeric vector for specifying the object's velocity, where
#'                the first value specifies the velocity in the x-direction and
#'                the second in the y-direction [Default: c(0.0, 0.0)].
#' @slot active A length-one logical vector indicating whether the object is
#'              active [Default: TRUE].
#'
#' @param object An object of class "Agent"
#' @param location A numerical vector of length 2 specifying the x- and
#'                 y-position of the object [Default: c(0, 0)].
#' @param velocity A numerical vector of length 2 specifying the velocity in the
#'                 x- and y-direction of the object [Default: c(0, 0)].
#' @param active A length-one logical vector indicating whether the object is
#'               active (TRUE) or not (FALSE) [Default: TRUE].
#'
#' @seealso \code{\link{validAgentObject}}, \code{\link{initialize,Agent-method}}
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
#' is.object(a)
#' isS4(a)
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
