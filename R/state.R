bundled_S3_definitions <- new_union(
  class_data.frame,
  class_Date,
  class_factor,
  class_POSIXct,
  class_POSIXlt,
  class_POSIXt,
  class_formula
)

state_classes <- S7_object | bundled_S3_definitions | class_vector

type_to_class_name <- function(type) {
  class_name <- ""

  if (S7_inherits(type)) {
    pkg_name <- type@package
    class_name <- type@name
    if (!is.null(pkg_name)) {
      class_name <- paste0(pkg_name, "::", class_name)
    }
  } else {
    class_name <- type$class
  }
  class_name
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
    value = class_any,
    type = state_classes,
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
    if (!inherits(self@value, class_name)) {
      sprintf("`value` is not a <%s> object", class_name)
    }
  },
  package = "state"
)
