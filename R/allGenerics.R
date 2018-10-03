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
            if (validObject(.Object) == TRUE) {
              return(.Object)
            } else {
              stop("The velocity level is out of bounds.")
            }
          })

### GET Methods Definitions ----
### create a method to retreive (get) the value of a slot by name
#' Retrieve-method for slots
#'
#' Extract parts of an object of class Agent by slot name.
#'
#' @param x object of class "Agent"
#' @param i character, describing the slotname to retreive
setMethod(
  f = "[",
  signature = c(x = "Agent", i = "character"),
  definition = function(x, i) {
    if (i == "location") return(x@location)
    if (i == "velocity") return(x@velocity)
    if (i == "active") return(x@active)
  })
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
### create a method to assign (set) the value of a slot by name
#' Replace-method for slots
#'
#' Replace values of slots in an object of class Agent by name
#'
#' @param x object of class "Agent"
#' @param i character, describing the slotname to be replace with a value
#' @param value ANY, value for the slotname specified by i to be replaced
setReplaceMethod(
  f = "[",
  signature = c(x = "Agent", i = "character", value = "ANY"),
  definition = function(x, i, value) {
    if (i == "location") {
      x@location <- value
      validObject(x)
      return(x)
    }
    if (i == "velocity") {
      x@velocity <- value
      validObject(x)
      return(x)
    }
    if (i == "active")  {
      x@active <- value
      validObject(x)
      return(x)
    }
  })
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
setMethod(f = "setLocation", signature = "Agent",
          definition = function(object, location) {
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

### MOVE Methods Definitions ----
### create a method to move object of class Agent, Prey, Bobcat and Lynx
#' move Generic
#'
#' Generic function to move an object of a defined class.
#'
#' @param object An object of a defined S4 class (\linkS4class{Agent},
#'               \linkS4class{Prey}, \linkS4class{Bobcat}. or \linkS4class{Lynx}).
#'
#' @export
setGeneric(name = "move",
           def = function(object) {
             standardGeneric("move")
           }
          )
#' @describeIn move Move an object of class \linkS4class{Agent}.
setMethod(f = "move",
          signature = "Agent",
          definition = function(object) {
            print("Move this Agent dude")
            object <- setVelocity(object, velocity = c(1, 2))
            validObject(object)
            return(object)
          }
         )
#' @describeIn move Move an object of class \linkS4class{Prey}.
setMethod(f = "move",
          signature = "Prey",
          definition = function(object) {
            print("Check this Prey before moving this dude")
            object <- callNextMethod(object)
            print("Move this Prey dude")
            validObject(object)
            return(object)
          }
         )
#' @describeIn move Move an object of class \linkS4class{Bobcat}.
setMethod(f = "move",
          signature = "Bobcat",
          definition = function(object) {
            print("Check this Bobcat before moving this dude")
            object <- setLocation(object, location = c(2, 3))
            object <- callNextMethod(object)
            print("Move this Bobcat dude")
            validObject(object)
            return(object)
          }
         )
#' @describeIn move Move an object of class \linkS4class{Bobcat}.
setMethod(f = "move",
          signature = "Lynx",
          definition = function(object) {
            print("Check this Lynx before moving this dude")
            object <- setActive(object, active = FALSE)
            object <- callNextMethod(object)
            print("Move this Lynx dude")
            validObject(object)
            return(object)
          }
         )
