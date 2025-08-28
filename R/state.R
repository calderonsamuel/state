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

is_S7_union <- function(x) inherits(x, "S7_union")

get_S7_union_class_names <- function(x) {
  non_nulls <- Filter(Negate(is.null), x$classes)
  vapply(non_nulls, function(.x) .x$class, character(1L))
}

S7_union_includes_NULL <- function(x) {
  nulls <- Filter(is.null, x$classes)
  length(nulls) > 0L
}

type_to_class_name <- function(type) {
  if (is_S7_union(type)) return(get_S7_union_class_names(type))
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
#'
#' @export
state <- new_class(
  name = "state",
  properties = list(
    value = state_classes,
    type = new_property(
      class = class_any,
      validator = function(value) {
        if (!inherits(value, c("S7_object", "S7_S3_class", "S7_base_class", "S7_union"))) {
          "should be a class supported by S7"
        }
      }
    )
  ),
  validator = function(self) {
    class_name <- type_to_class_name(self@type)

    if (!is_S7_union(self@type)) {
      if (inherits(self@value, class_name)) return()
      if (("double" %in% class_name) && is.double(self@value)) return()
      return(sprintf("`value` is not a <%s> object", class_name))
    }

    if (inherits(self@value, class_name)) return()
    if (("double" %in% class_name) && is.double(self@value)) return()

    union_class_names <- paste0(sprintf("<%s>", class_name), collapse = " or ")

    if (S7_union_includes_NULL(self@type)) {
      if (is.null(self@value)) return()
      return(sprintf("`value` is not a %s or %s object", "`NULL`", union_class_names))
    }

    return(sprintf("`value` is not a %s object", union_class_names))
  },
  package = "state"
)
