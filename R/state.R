bundled_S3_definitions <- new_union(
  class_data.frame,
  class_Date,
  class_factor,
  class_POSIXct,
  class_POSIXlt,
  class_POSIXt,
  class_formula
)

#'
#' @export
state <- new_class(
  name = "state",
  properties = list(
    value = class_any,
    type = S7_object | bundled_S3_definitions | class_vector,
    allow_null = new_property(
      class = class_logical,
      default = FALSE,
      validator = function(value) {
        if (length(value) != 1L || is.na(value)) "Should be TRUE or FALSE"
      }
    )
  ),
  validator = function(self) {
    if (!inherits(self@value, self@type)) {
      class_name <- if (S7_inherits(self@type)) self@type@name else self@type$class
      sprintf("`value` is not a <%s> object", class_name)
    }
  }
)
