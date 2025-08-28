# =============================================================================
# Basic Initialization Tests
# =============================================================================

test_that("state_manager initializes with named states", {
  sm <- state_manager(
    x = state(1L, class_integer),
    y = state("a", class_character)
  )
  expect_s3_class(sm, "state_manager")
  expect_equal(shiny::isolate(sm$x), 1L)
  expect_equal(shiny::isolate(sm$y), "a")
})

test_that("state_manager initializes empty", {
  sm <- state_manager()
  expect_s3_class(sm, "state_manager")
  expect_length(shiny::isolate(shiny::reactiveValuesToList(sm)), 0)
})

# =============================================================================
# Assignment and Access Tests
# =============================================================================

test_that("initial update with matching type succeeds", {
  sm <- state_manager(x = state(1L, class_integer))
  sm$x <- 2L
  expect_equal(shiny::isolate(sm$x), 2L)
})

test_that("initial update with wrong type fails", {
  sm <- state_manager(x = state(1L, class_integer))
  expect_error(sm$x <- "2", "Invalid type")
})

test_that("$<- and [[<- assignment operators work identically", {
  sm1 <- state_manager(x = state(1L, class_integer))
  sm2 <- state_manager(x = state(1L, class_integer))

  sm1$x <- 10L
  sm2[["x"]] <- 10L

  expect_equal(shiny::isolate(sm1$x), shiny::isolate(sm2[["x"]]))
})

test_that("[[<- assignment works for updates", {
  sm <- state_manager(x = state(1L, class_integer))
  sm[["x"]] <- 5L
  expect_equal(shiny::isolate(sm[["x"]]), 5L)
})

test_that("state can be defined after creation", {
  sm <- state_manager()
  sm$x <- state(1L, class_integer)
  expect_equal(shiny::isolate(sm$x), 1L)
})

test_that("assigning raw value without defining state fails", {
  sm <- state_manager()
  expect_error(sm$x <- 1L, "No type defined")
  expect_error(sm[["y"]] <- "test", "No type defined")
})

test_that("redefining state with another state object fails", {
  sm <- state_manager(x = state(1L, class_integer))
  expect_error(sm$x <- state("oops", class_character), "Cannot reassign")
})

# =============================================================================
# Argument Validation Tests
# =============================================================================

test_that("state_manager rejects unnamed arguments", {
  expect_error(
    state_manager(state(1L, class_integer)),
    "All arguments passed to state_manager\\(\\) must be named\\."
  )
})

test_that("state_manager rejects mixed named/unnamed arguments", {
  expect_error(
    state_manager(x = state(1L, class_integer), state(2L, class_integer)),
    "All arguments passed to state_manager\\(\\) must be named\\."
  )
})

test_that("state_manager rejects empty string names", {
  args <- list(state(1L, class_integer))
  names(args) <- ""

  expect_error(
    do.call(state_manager, args),
    "All arguments passed to state_manager\\(\\) must be named\\."
  )
})

test_that("non-state objects are rejected in state_manager", {
  expect_error(
    state_manager(x = 1L),
    "`x` is not a <state> object\\. Use `state\\(\\)` when creating typed reactive values\\."
  )
})

# =============================================================================
# Type Validation and Error Handling Tests
# =============================================================================

test_that("class mismatch gives informative error", {
  sm <- state_manager(x = state(1L, class_integer))
  err <- expect_error(sm$x <- "text")
  expect_match(err$message, "expected <integer>, got <character>")
})

test_that("type mismatch errors are comprehensive", {
  sm <- state_manager(
    x = state(1L, class_integer),
    y = state("test", class_character),
    z = state(TRUE, class_logical)
  )

  # Test all type mismatches with exact error messages
  expect_error(sm$x <- "text", "Invalid type for 'x': expected <integer>, got <character>\\.")
  expect_error(sm$x <- TRUE, "Invalid type for 'x': expected <integer>, got <logical>\\.")

  expect_error(sm$y <- 1L, "Invalid type for 'y': expected <character>, got <integer>\\.")
  expect_error(sm$y <- TRUE, "Invalid type for 'y': expected <character>, got <logical>\\.")

  expect_error(sm$z <- 1L, "Invalid type for 'z': expected <logical>, got <integer>\\.")
  expect_error(sm$z <- "text", "Invalid type for 'z': expected <logical>, got <character>\\.")
})

test_that("type mismatch errors respects unions", {
  sm <- state_manager(
    x = state(1L, class_integer | class_character),
    y = state(NULL, NULL | class_integer)
  )

  expect_error(
    {
      sm$x <- TRUE
    },
    regexp = "Invalid type for 'x': expected <integer> or <character>, got <logical>"
  )

  expect_error(
    {
      sm$y <- TRUE
    },
    regexp = "Invalid type for 'y': expected `NULL` or <integer>, got <logical>"
  )
})

test_that("undefined state assignment gives exact error", {
  sm <- state_manager()
  expect_error(sm$x <- 1L, "No type defined for 'x'\\. Assign a <state> object first\\.")
  expect_error(sm[["y"]] <- "test", "No type defined for 'y'\\. Assign a <state> object first\\.")
})

test_that("state redefinition gives exact error", {
  sm <- state_manager(x = state(1L, class_integer))
  expect_error(
    sm$x <- state("oops", class_character),
    "Cannot reassign 'x' with a <state>. It is already defined\\."
  )
})

# =============================================================================
# Data Type and Vector Tests
# =============================================================================

test_that("state works with empty vectors", {
  sm <- state_manager(
    x = state(integer(), class_integer),
    y = state(character(), class_character),
    z = state(logical(), class_logical)
  )

  expect_equal(shiny::isolate(sm$x), integer())
  expect_equal(shiny::isolate(sm$y), character())
  expect_equal(shiny::isolate(sm$z), logical())

  # Empty states can be updated
  sm$x <- 5L
  expect_equal(shiny::isolate(sm$x), 5L)

  sm$x <- integer()  # Back to empty
  expect_equal(shiny::isolate(sm$x), integer())
})

# =============================================================================
# Class Preservation and State Management Tests
# =============================================================================

test_that("state_manager class is preserved after operations", {
  sm <- state_manager(x = state(1L, class_integer))
  expect_s3_class(sm, "state_manager")

  sm$x <- 2L
  expect_s3_class(sm, "state_manager")

  sm$y <- state("new", class_character)
  expect_s3_class(sm, "state_manager")
})

test_that("type information is preserved across operations", {
  sm <- state_manager(
    x = state(1L, class_integer),
    y = state("test", class_character)
  )

  sm$x <- 10L
  sm$y <- "updated"

  # Types should still be accessible
  expect_true(attr(sm, "types")$has("x"))
  expect_true(attr(sm, "types")$has("y"))
  expect_equal(attr(sm, "types")$get("x"), class_integer)
  expect_equal(attr(sm, "types")$get("y"), class_character)
})

# =============================================================================
# Complex Integration Tests
# =============================================================================

test_that("complex multi-step operations work correctly", {
  # Start empty
  sm <- state_manager()

  # Add multiple states
  sm$a <- state(1L, class_integer)
  sm$b <- state("initial", class_character)
  sm$c <- state(FALSE, class_logical)

  # Verify initial state
  expect_equal(shiny::isolate(sm$a), 1L)
  expect_equal(shiny::isolate(sm$b), "initial")
  expect_equal(shiny::isolate(sm$c), FALSE)

  # Update all values
  sm$a <- 100L
  sm$b <- "updated"
  sm$c <- TRUE

  # Verify updates
  expect_equal(shiny::isolate(sm$a), 100L)
  expect_equal(shiny::isolate(sm$b), "updated")
  expect_equal(shiny::isolate(sm$c), TRUE)

  # Try invalid updates (should fail)
  expect_error(sm$a <- "invalid")
  expect_error(sm$b <- 123L)
  expect_error(sm$c <- "invalid")

  # Values should be unchanged after failed updates
  expect_equal(shiny::isolate(sm$a), 100L)
  expect_equal(shiny::isolate(sm$b), "updated")
  expect_equal(shiny::isolate(sm$c), TRUE)
})

test_that("alternating $ and [[ operations work correctly", {
  sm <- state_manager()

  # Mix assignment operators
  sm$x <- state(1L, class_integer)
  sm[["y"]] <- state("test", class_character)
  sm$z <- state(TRUE, class_logical)

  # Mix access patterns
  sm[["x"]] <- 10L
  sm$y <- "updated"
  sm[["z"]] <- FALSE

  # Verify final state
  expect_equal(shiny::isolate(sm$x), 10L)
  expect_equal(shiny::isolate(sm[["y"]]), "updated")
  expect_equal(shiny::isolate(sm$z), FALSE)
})
