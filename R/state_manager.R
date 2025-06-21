#' Typed reactiveValues
#'
#' A `shiny::reactiveValues` object with type safety features.
#' All elements must be named and initialized with `state()`
#'
#' @inheritParams shiny::reactiveValues
#'
#' @export
state_manager <- function(...) {
  args <- rlang::list2(...)

  if (!rlang::is_named2(args)) {
    stop("All arguments passed to state_manager() must be named.")
  }

  Map(check_is_state, args, names(args))

  values <- lapply(args, \(x) x@value)
  rv <- shiny::reactiveValues(!!!values)

  types <- lapply(args, \(x) x@type)
  fm <- fastmap::fastmap()
  fm$mset(.list = types)
  attr(rv, "types") <- fm
  class(rv) <- c("state_manager", class(rv))
  return(rv)
}

#' @export
`$<-.state_manager` <- function(x, name, value) {
  types <- attr(x, "types")

  if (inherits(value, state)) {
    x <- set_new_state(x, name, value, types)
  } else {
    x <- update_existing_state(x, name, value, types)
  }

  attr(x, "types") <- types
  x
}

#' @export
`[[<-.state_manager` <- `$<-.state_manager`

check_is_state <- function(.value, .name) {
  if (!inherits(.value, state)) {
    stop(sprintf("`%s` is not a <state> object. Use `state()` when creating typed reactive values.", .name))
  }
}

set_new_state <- function(x, name, state_obj, types) {
  if (types$has(name)) {
    abort_already_defined(name)
  }

  types$set(name, state_obj@type)

  with_stripped_class(x, function(x_stripped) {
    x_stripped[[name]] <- state_obj@value
    x_stripped
  }, class(x))
}

update_existing_state <- function(x, name, value, types) {
  if (!types$has(name)) {
    abort_not_defined(name)
  }

  expected_type <- types$get(name)

  expected_type_class <- type_to_class_name(expected_type)

  if (!inherits(value, expected_type_class)) {
    abort_type_mismatch(name, expected_type, class(value)[1])
  }

  with_stripped_class(x, function(x_stripped) {
    x_stripped[[name]] <- value
    x_stripped
  }, class(x))
}

with_stripped_class <- function(x, f, original_class) {
  class(x) <- setdiff(original_class, "state_manager")
  x <- f(x)
  class(x) <- original_class
  x
}

abort_already_defined <- function(name) {
  stop(sprintf("Cannot reassign '%s' with a <state>: it is already defined.", name), call. = FALSE)
}

abort_not_defined <- function(name) {
  stop(sprintf("No type defined for '%s'. Assign a <state> object first.", name), call. = FALSE)
}

abort_type_mismatch <- function(name, expected, actual) {
  expected_class <- type_to_class_name(expected)
  stop(sprintf("Invalid type for '%s': expected <%s>, got <%s>.", name, expected_class, actual), call. = FALSE)
}
