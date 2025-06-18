#' @export
state <- new_class(
  name = "state",
  properties = list(
    value = class_any,
    type = class_any
  ),
  validator = function(self) {
    if (!inherits(self@value, self@type)) {
      sprintf("`value` is not a <%s>", self@type$class)
    }
  }
)
