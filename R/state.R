# ./state.R

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

  if (!inherits(value, expected_type)) {
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
  stop(sprintf("Invalid type for '%s': expected <%s>, got <%s>.", name, expected$class, actual), call. = FALSE)
}

int_state <- function(x = integer()) state(x, class_integer)
chr_state <- function(x = character()) state(x, class_character)
lgl_state <- function(x = logical()) state(x, class_logical)

