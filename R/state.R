bundled_S3_definitions <- new_union(
  class_data.frame,
  class_Date,
  class_factor,
  class_POSIXct,
  class_POSIXlt,
  class_POSIXt,
  class_formula
)

state_classes <- NULL | S7_object | bundled_S3_definitions | class_vector

type_to_class_name <- function(type) {
  if (!S7_inherits(type)) return(type$class)
  if (is.null(type@package)) return(type@name)

  paste0(type@package, "::", type@name)
}

#' State
#'
#' A way of defining state for a shiny application. Alternative to `shiny::reactiveValues()`.
#'
#' @param value Initial value of the state
#' @param type The expected class
#' @param allow_null Is it allowed for the value to be NULL?
#'
#' @export
state <- new_class(
  name = "state",
  properties = list(
    value = state_classes,
    type = new_property(
      class = class_any,
      validator = function(value) {
        if (!inherits(value, c("S7_object", "S7_S3_class", "S7_base_class"))) {
          "should be a class supported by S7"
        }
      }
    ),
    allow_null = new_property(
      class = class_logical,
      default = FALSE,
      validator = function(value) {
        if (length(value) != 1L || is.na(value)) "Should be TRUE or FALSE"
      }
    )
  ),
  validator = function(self) {
    class_name <- type_to_class_name(self@type)
    if (inherits(self@value, class_name)) return()
    if (identical(class_name, "double") && is.double(self@value)) return()
    sprintf("`value` is not a <%s> object", class_name)
  },
  package = "state"
)
