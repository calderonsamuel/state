# ./tests/testthat/test-state.R

test_that("state_manager initializes with named states", {
  sm <- state_manager(x = int_state(1L), y = chr_state("a"))
  expect_s3_class(sm, "state_manager")
  expect_equal(shiny::isolate(sm$x), 1L)
  expect_equal(shiny::isolate(sm$y), "a")
})

test_that("initial update with matching type succeeds", {
  sm <- state_manager(x = int_state(1L))
  sm$x <- 2L
  expect_equal(shiny::isolate(sm$x), 2L)
})

test_that("initial update with wrong type fails", {
  sm <- state_manager(x = int_state(1L))
  expect_error(sm$x <- "2", "Invalid type")
})

test_that("state can be defined after creation", {
  sm <- state_manager()
  sm$x <- int_state(1L)
  expect_equal(shiny::isolate(sm$x), 1L)
  sm$x <- 5L
  expect_equal(shiny::isolate(sm$x), 5L)
})

test_that("assigning raw value without defining state fails", {
  sm <- state_manager()
  expect_error(sm$x <- 1L, "No type defined")
})

test_that("redefining state with another state object fails", {
  sm <- state_manager(x = int_state(1L))
  expect_error(sm$x <- chr_state("oops"), "Cannot reassign")
})

test_that("class mismatch gives informative error", {
  sm <- state_manager(x = int_state(1L))
  err <- expect_error(sm$x <- "text")
  expect_match(err$message, "expected <integer>, got <character>")
})

test_that("logical and character states work", {
  sm <- state_manager(
    a = lgl_state(TRUE),
    b = chr_state("hello")
  )

  sm$a <- FALSE
  sm$b <- "world"

  expect_equal(shiny::isolate(sm$a), FALSE)
  expect_equal(shiny::isolate(sm$b), "world")
})

test_that("multiple entries and overwrites work properly", {
  sm <- state_manager()
  sm$x <- int_state(10L)
  sm$y <- chr_state("x")
  sm$z <- lgl_state(TRUE)

  sm$x <- 15L
  sm$y <- "updated"
  sm$z <- FALSE

  expect_equal(shiny::isolate(sm$x), 15L)
  expect_equal(shiny::isolate(sm$y), "updated")
  expect_equal(shiny::isolate(sm$z), FALSE)
})

test_that("invalid class gets caught by validator", {
  expect_error(state("a", class_integer), "not a <integer>")
})

test_that("validator triggers inside state_manager construction", {
  expect_error(state_manager(x = state("a", class_integer)), "not a <integer>")
})
