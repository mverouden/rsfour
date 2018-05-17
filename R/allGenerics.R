### Initialize Classes ----
#' Initialize an object of class "Agent".
#'
#' The initialize function sets the values for the slots of an object of class
#' \linkS4class{Agent}.
#'
#' @param .Object An object of class \linkS4class{Agent}.
#' @param location A numerical vector of length 2 specifying the x- and
#'                 y-position of the object of class \linkS4class{Agent}
#'                 [Default: c(0, 0)].
#' @param velocity A numerical vector of length 2 specifying the velocity in the
#'                 x- and y-direction of the object of class \linkS4class{Agent}
#'                 [Default: c(0, 0)].
#' @param active A length-one logical vector indicating whether the object of
#'               class \linkS4class{Agent} is active (TRUE) or not (FALSE)
#'               [Default: TRUE].
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
#'                 active = FALSE)
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
              stop("The location argument should be length 2.")
            } else {
              .Object@location <- location
            }
            if (length(velocity) == 0) {
              .Object@velocity <- c(0.0, 0.0)
            } else if (length(velocity) < 2 | length(velocity) > 2) {
              stop("The velocity argument should be length 2.")
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

### GET Methods Definitions ----
### create a method to retreive (get) the value of the location slot
#' getLocation Generic
#'
#' Generic function definition for retreiving the location slot from an object
#' of class \linkS4class{Agent}.
#'
#' @param object An object of class \linkS4class{Agent}
setGeneric(name = "getLocation",
           def = function(object) {
             standardGeneric("getLocation")
           }
          )
#' @describeIn Agent Retreive the location slot of an object of class "Agent".
#' @export
setMethod(f = "getLocation",
          signature = "Agent",
          definition = function(object) {
            return(object@location)
          }
         )

### create a method to retreive (get) the value of the velocity slot
#' getVelocity Generic
#'
#' Generic function definition for retreiving the velocity slot from an object
#' of class \linkS4class{Agent}.
#'
#' @param object An object of class \linkS4class{Agent}
setGeneric(name = "getVelocity",
           def = function(object) {
             standardGeneric("getVelocity")
           }
          )
#' @describeIn Agent Retreive the velocity slot of an object of class "Agent".
#' @export
setMethod(f = "getVelocity",
          signature = "Agent",
          definition = function(object) {
            return(object@velocity)
          }
         )

### create a method to retreive (get) the value of active slot
#' getActive Generic
#'
#' @param object An object of class \linkS4class{Agent}
setGeneric(name = "getActive",
           def = function(object) {
             standardGeneric("getActive")
           }
          )
#' @describeIn Agent Retreive the active slot of an object of class "Agent".
#' @export
setMethod(f = "getActive",
          signature = "Agent",
          definition = function(object) {
            return(object@active)
          }
         )

### SET Methods Definitions ----
### create a method to assign (set) the value of the location
#' setLocation Generic
#'
#' @param object An object of class \linkS4class{Agent}
#' @param location A numerical vector of length 2 specifying the x- and
#'                 y-position of the object of class \linkS4class{Agent}
#'                 [Default: c(0, 0)].
setGeneric(name = "setLocation",
           def = function(object, location) {
             standardGeneric("setLocation")
           }
          )
#' @describeIn Agent Assign the location slot of an object of class "Agent".
#' @export
setMethod(f = "setLocation", signature = "Agent", definition = function(object, location) {
            object@location <- location
            validObject(object)
            return(object)
          }
         )

### create a method to assign (set) the value of velocity
#' setVelocity Generic
#'
#' @param object An object of class \linkS4class{Agent}
#' @param velocity A numerical vector of length 2 specifying the velocity in the
#'                 x- and y-direction of the object of class \linkS4class{Agent}
#'                 [Default: c(0, 0)].
setGeneric(name = "setVelocity",
           def = function(object, velocity) {
             standardGeneric("setVelocity")
           }
          )
#' @describeIn Agent Assign the velocity slot of an object of class "Agent".
#' @export
setMethod(f = "setVelocity",
          signature = "Agent",
          definition = function(object, velocity) {
            object@velocity <- velocity
            validObject(object)
            return(object)
          }
         )

### create a method to assign (set) the value of active
#' setAcitve Generic
#'
#' Generic function to set the active slot of an object of class
#' \linkS4class{Agent}.
#'
#' @param object An object of class \linkS4class{Agent}
#' @param active A length-one logical vector indicating whether the object of
#'               class \linkS4class{Agent} is active (TRUE) or not (FALSE)
#'               [Default: TRUE].
setGeneric(name = "setActive",
           def = function(object, active) {
             standardGeneric("setActive")
           }
          )
#' @describeIn Agent Assign the active slot of an object of class "Agent".
#' @export
setMethod(f = "setActive",
          signature = "Agent",
          definition = function(object, active) {
            object@active <- active
            validObject(object)
            return(object)
          }
         )

### RESET Methods Definitions ----
### create a method to reset the velocity and the activity
#' resetActivity Generic
#'
#' Generic function to reset the activity of an object of class
#' \linkS4class{Agent}.
#'
#' @param object An object of class \linkS4class{Agent}
#' @param value Possible values:\itemize{
#'               \item a length-one logical vector to reset the active slot of
#'              the \code{\link{Agent-class}} object.
#'               \item a numerical vector of
#'              length 2 to reset the velocity in the x- and y-direction of the
#'              object of class \linkS4class{Agent}.
#'              }
#'
#' @export
setGeneric(name = "resetActivity",
           def = function(object, value) {
             standardGeneric("resetActivity")
           }
          )
#' @describeIn resetActivity Reset the active slot to the specified value and reset the velocity slot to c(0, 0).
setMethod(f = "resetActivity",
          signature = c("Agent","logical"),
          definition = function(object, value) {
            object <- setActive(object, value)
            object <- setVelocity(object, c(0.0, 0.0))
            return(object)
          }
         )
#' @describeIn resetActivity Reset the velocity slot to the specified values and reset the active slot to TRUE.
setMethod(f = "resetActivity",
          signature = c("Agent", "numeric"),
          definition = function(object, value) {
            object <- setActive(object, TRUE)
            object <- setVelocity(object, value)
            return(object)
          }
         )
