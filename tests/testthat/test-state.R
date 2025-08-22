# =============================================================================
# S7 Class System Tests
# =============================================================================

test_that("base classes can be initialized", {
  expect_no_error(state("a", class_character))
  expect_no_error(state(TRUE, class_logical))
  expect_no_error(state(1L, class_integer))
  expect_no_error(state(1.0, class_double))
  expect_no_error(state(list(), class_list))
})

test_that("invalid class gets caught by validator", {
  expect_error(state("a", class_integer), "not a <integer>")
})

test_that("validator triggers inside state_manager construction", {
  expect_error(state_manager(x = state("a", class_integer)), "not a <integer>")
})

test_that("state class validation works correctly", {
  # Valid states
  expect_true(S7_inherits(state(1L, class_integer), state))
  expect_true(S7_inherits(state("test", class_character), state))
  expect_true(S7_inherits(state(TRUE, class_logical), state))

  # Invalid states trigger validator
  expect_error(state("text", class_integer), "`value` is not a <integer>")
  expect_error(state(1L, class_character), "`value` is not a <character>")
  expect_error(state("text", class_logical), "`value` is not a <logical>")
  expect_error(state("text", class_double), "`value` is not a <double>")
})

test_that("state properties are accessible", {
  s <- state(42L, class_integer)
  expect_equal(s@value, 42L)
  expect_equal(s@type, class_integer)
})

test_that("state constructor works with various inputs", {
  # Single values
  s1 <- state(5L, class_integer)
  expect_equal(s1@value, 5L)
  expect_equal(s1@type, class_integer)

  s2 <- state("hello", class_character)
  expect_equal(s2@value, "hello")
  expect_equal(s2@type, class_character)

  s3 <- state(TRUE, class_logical)
  expect_equal(s3@value, TRUE)
  expect_equal(s3@type, class_logical)

  # Vectors
  s4 <- state(c(1L, 2L, 3L), class_integer)
  expect_equal(s4@value, c(1L, 2L, 3L))

  s5 <- state(c("a", "b", "c"), class_character)
  expect_equal(s5@value, c("a", "b", "c"))

  s6 <- state(c(TRUE, FALSE, TRUE), class_logical)
  expect_equal(s6@value, c(TRUE, FALSE, TRUE))
})

# =============================================================================
# Custom S7 Class Tests
# =============================================================================

test_that("state_manager works with custom S7 classes with package", {
  Person <- new_class(
    name = "Person",
    properties = list(
      name = class_character,
      age = class_integer
    )
  )

  john <- Person(name = "John", age = 30L)
  person <- state(john, Person)

  expect_equal(person@value@name, "John")
  expect_equal(person@value@age, 30L)
})

test_that("state_manager works with custom S7 classes without package", {
  Person <- new_class(
    name = "Person",
    properties = list(
      name = class_character,
      age = class_integer
    ),
    package = NULL
  )

  john <- Person(name = "John", age = 30L)
  person <- state(john, Person)

  expect_equal(person@value@name, "John")
  expect_equal(person@value@age, 30L)
})

test_that("custom S7 class state can be updated", {
  Person <- new_class("Person",
                      properties = list(
                        name = class_character,
                        age = class_integer
                      )
  )

  john <- Person(name = "John", age = 30L)
  jane <- Person(name = "Jane", age = 25L)

  sm <- state_manager(person = state(john, Person))
  sm$person <- jane

  expect_equal(shiny::isolate(sm$person@name), "Jane")
  expect_equal(shiny::isolate(sm$person@age), 25L)
})

test_that("wrong type assignment to custom S7 class fails", {
  Person <- new_class(
    name = "Person",
    properties = list(name = class_character),
    package = NULL
  )

  john <- Person(name = "John")
  sm <- state_manager(person = state(john, Person))

  expect_error(sm$person <- "not a person", "Invalid type.*expected <Person>")
  expect_error(sm$person <- 123L, "Invalid type.*expected <Person>")
})

test_that("S7 class inheritance works with state", {
  Animal <- new_class("Animal",
                      properties = list(name = class_character)
  )

  Dog <- new_class("Dog",
                   parent = Animal,
                   properties = list(breed = class_character)
  )

  buddy <- Dog(name = "Buddy", breed = "Golden Retriever")

  # Dog should be accepted where Animal is expected
  sm <- state_manager(animal = state(buddy, Animal))
  expect_equal(shiny::isolate(sm$animal@name), "Buddy")
})

# =============================================================================
# Complex Data Type Tests
# =============================================================================

test_that("state handles data.frame objects", {
  df <- data.frame(
    x = 1:3,
    y = c("a", "b", "c"),
    stringsAsFactors = FALSE
  )

  expect_equal(state(df, class_data.frame)@value, df)
})

test_that("data.frame state can be updated", {
  df1 <- data.frame(x = 1:2, y = c("a", "b"))
  df2 <- data.frame(x = 3:4, y = c("c", "d"))

  sm <- state_manager(data = state(df1, class_data.frame))
  sm$data <- df2

  expect_equal(shiny::isolate(sm$data), df2)
})

test_that("non-data.frame assignment to data.frame state fails", {
  df <- data.frame(x = 1:2)
  sm <- state_manager(data = state(df, class_data.frame))

  expect_error(sm$data <- list(x = 1:2), "Invalid type.*expected <data.frame>")
  expect_error(sm$data <- matrix(1:4, nrow = 2), "Invalid type.*expected <data.frame>")
})

test_that("state handles list objects", {
  lst <- list(
    numbers = 1:3,
    text = c("a", "b"),
    nested = list(inner = TRUE)
  )

  sm <- state_manager(my_list = state(lst, class_list))
  expect_equal(shiny::isolate(sm$my_list), lst)
})

test_that("list state can be updated", {
  lst1 <- list(a = 1, b = 2)
  lst2 <- list(x = "hello", y = "world")

  sm <- state_manager(my_list = state(lst1, class_list))
  sm$my_list <- lst2

  expect_equal(shiny::isolate(sm$my_list), lst2)
})

test_that("non-list assignment to list state fails", {
  lst <- list(a = 1, b = 2)
  sm <- state_manager(my_list = state(lst, class_list))

  expect_error(sm$my_list <- c(1, 2, 3), "Invalid type.*expected <list>")
  expect_error(sm$my_list <- data.frame(x = 1), "Invalid type.*expected <list>")
})

test_that("state handles factor objects", {
  fac <- factor(c("low", "medium", "high"), levels = c("low", "medium", "high"))
  sm <- state_manager(my_factor = state(fac, class_factor))
  expect_equal(shiny::isolate(sm$my_factor), fac)
})

test_that("factor state preserves levels", {
  fac1 <- factor(c("a", "b"), levels = c("a", "b", "c"))
  fac2 <- factor(c("b", "c"), levels = c("a", "b", "c"))

  sm <- state_manager(my_factor = state(fac1, class_factor))
  sm$my_factor <- fac2

  result <- shiny::isolate(sm$my_factor)
  expect_equal(result, fac2)
  expect_equal(levels(result), c("a", "b", "c"))
})

# =============================================================================
# Numeric vs Integer Edge Cases
# =============================================================================

test_that("integer state rejects numeric values", {
  sm <- state_manager(x = state(1L, class_integer))
  expect_error(sm$x <- 1.0, "Invalid type.*expected <integer>, got <numeric>")
  expect_error(sm$x <- 1.5, "Invalid type.*expected <integer>, got <numeric>")
})

# =============================================================================
# Error Recovery Tests
# =============================================================================

test_that("state_manager remains functional after failed assignments", {
  sm <- state_manager(x = state(1L, class_integer))

  # Attempt invalid assignment
  expect_error(sm$x <- "invalid")

  # State should be unchanged and still functional
  expect_equal(shiny::isolate(sm$x), 1L)

  # Should still accept valid assignments
  sm$x <- 42L
  expect_equal(shiny::isolate(sm$x), 42L)
})

test_that("multiple failed assignments don't corrupt state", {
  sm <- state_manager(
    x = state(1L, class_integer),
    y = state("test", class_character)
  )

  # Multiple failed attempts
  expect_error(sm$x <- "fail1")
  expect_error(sm$y <- 123L)
  expect_error(sm$x <- TRUE)

  # Original values should be preserved
  expect_equal(shiny::isolate(sm$x), 1L)
  expect_equal(shiny::isolate(sm$y), "test")

  # Should still work for valid assignments
  sm$x <- 99L
  sm$y <- "updated"
  expect_equal(shiny::isolate(sm$x), 99L)
  expect_equal(shiny::isolate(sm$y), "updated")
})

test_that("type metadata remains consistent after errors", {
  sm <- state_manager(x = state(1L, class_integer))

  expect_error(sm$x <- "invalid")

  # Type information should still be intact
  types <- attr(sm, "types")
  expect_true(types$has("x"))
  expect_equal(types$get("x"), class_integer)

  # Class should be preserved
  expect_s3_class(sm, "state_manager")
})

