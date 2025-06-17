state_manager <- function(...) {
  args <- list2(...)

  if ((length(args) > 0) && (is.null(names(args)) || any(names(args) == ""))) {
    rlang::abort("All arguments passed to state_manager() must be named.")
  }

  withCallingHandlers(
    purrr::imap(args, check_is_state),
    purrr_error_indexed = function(err) {
      rlang::cnd_signal(err$parent)
    }
  )

  values <- lapply(args, \(x) x@value)
  rv <- shiny::reactiveValues(!!!values)

  types <- lapply(args, \(x) x@type)
  fm <- fastmap::fastmap()
  fm$mset(.list = types)
  attr(rv, "types") <- fm
  class(rv) <- c("state_manager", class(rv))
  return(rv)
}

check_is_state <- function(.value, .name) {
  if (!inherits(.value, state)) {
    rlang::abort(sprintf("`%s` is not a <state> object. Use `state()` when creating typed reactive values.", .name))
  }
}

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


`$<-.state_manager` <- function(x, name, value) {
  types <- attr(x, "types")

  if (inherits(value, "state")) {
    # Prevent overwriting a declared state slot
    if (types$has(name)) {
      stop(sprintf("Cannot reassign '%s' with a <state>: it is already defined.", name), call. = FALSE)
    }

    # Set up a new reactive slot with defined type
    expected_type <- value@type
    real_value <- value@value

    types$set(name, expected_type)

    old_class <- class(x)
    class(x) <- setdiff(old_class, "state_manager")
    x[[name]] <- real_value
    class(x) <- old_class

  } else {
    # Ensure the type has already been defined
    if (!types$has(name)) {
      stop(sprintf("No type defined for '%s'. Assign a <state> object first.", name), call. = FALSE)
    }

    expected_type <- types$get(name)

    if (!inherits(value, expected_type)) {
      stop(sprintf("Invalid type for '%s': expected <%s>, got <%s>.",
                   name, expected_type@class, class(value)[1]), call. = FALSE)
    }

    old_class <- class(x)
    class(x) <- setdiff(old_class, "state_manager")
    x[[name]] <- value
    class(x) <- old_class
  }

  attr(x, "types") <- types
  x
}
