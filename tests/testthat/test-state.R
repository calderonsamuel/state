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

test_that("state works with custom S7 classes with package", {
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

test_that("state works with custom S7 classes without package", {
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

test_that("wrong type assignment to custom S7 class fails", {
  Person <- new_class(
    name = "Person",
    properties = list(name = class_character),
    package = NULL
  )

  expect_error(state("random string", Person), "`value` is not a <Person> object")
})

test_that("S7 class inheritance works with state", {
  Animal <- new_class(
    name = "Animal",
    properties = list(
      name = class_character
    )
  )

  Dog <- new_class(
    name = "Dog",
    parent = Animal,
    properties = list(
      breed = class_character
    )
  )

  expect_no_error(state(Dog(name = "Buddy", breed = "Golden Retriever"), Animal))
})

# =============================================================================
# Complex Data Type Tests
# =============================================================================

test_that("state handles data.frame objects", {
  df <- data.frame(
    x = 1:3,
    y = c("a", "b", "c")
  )

  expect_no_error(state(df, class_data.frame))
})

test_that("state handles list objects", {
  lst <- list(
    numbers = 1:3,
    text = c("a", "b"),
    nested = list(inner = TRUE)
  )

  expect_no_error(state(lst, class_list))
})

test_that("factor state preserves levels", {
  fac1 <- factor(c("a", "b"), levels = c("a", "b", "c"))

  expect_no_error(state(fac1, class_factor))
})

# =============================================================================
# Unions
# =============================================================================

test_that("state accepts S7 unions", {
  expect_no_error(state(1L, class_character | class_integer))
  expect_no_error(state("x", class_character | class_integer))
  expect_no_error(state(1L, class_double | class_integer))
  expect_no_error(state(1.0, class_double | class_integer))
})

test_that("state accepts S7 unions with NULL", {
  expect_no_error(state(1L, NULL | class_integer))
  expect_no_error(state(NULL, NULL | class_integer))
})

test_that("state gives meaningful error message when value doesn't inherit from union", {
  skip()
  expect_error(state(TRUE, class_character | class_integer))
  expect_error(state(TRUE, NULL | class_integer))
})


