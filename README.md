
<!-- README.md is generated from README.Rmd. Please edit that file -->

# state

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/state)](https://CRAN.R-project.org/package=state)
[![R-CMD-check](https://github.com/calderonsamuel/state/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/calderonsamuel/state/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/calderonsamuel/state/graph/badge.svg)](https://app.codecov.io/gh/calderonsamuel/state)
<!-- badges: end -->

The goal of state is to provide an alternative to
`shiny::reactiveValues()` that includes type safety functionality.

## Installation

You can install the development version of state like so:

``` r
pak::pak("calderonsamuel/state")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(state)
```

Create a state manager

``` r
sm <- state_manager()
```

Initialize a counter

``` r
sm$counter <- state(0L, S7::class_integer)
```

It will error if you try to assign a different type

``` r
sm$counter <- "new value"
#> Error: Invalid type for 'counter': expected <integer>, got <character>.
```

It will work if you assign a value of the expected class

``` r
sm$counter <- 1L
```

You can use it as a typical reactiveValues object

``` r
shiny::isolate(sm$counter)
#> [1] 1
```
