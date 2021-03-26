#' Define a metric
#' 
#' A metric is a definition of some comparison. By default,
#' there are two "metrics": integer distance, and string similarity.
#' This tool expands that toolkit to arbitrary comparisons.
#' 
#' @param variable A character vector of variables to run this metric on
#' @param func A function that takes a single argument. This is evaluated
#'   *by* entity ID.
#' @param tag A string to be used as the issue ID in the output spreadseet
#'   of `anara::verify_ids`
#' @return A 'metric' object
metric <- function(variable, func, tag) {
  stopifnot(is.character(variable))
  stopifnot(is.function(func))
  stopifnot(is.character(tag))

  structure(
    list(
      var = variable,
      f = func,
      tag = tag
    ),
    class = "metric"
  )
}

is_metric <- function(x) {
  inherits(x, "metric")
}

#' A collection of metrics
#' 
#' Primarily used in the `extra_metrics` parameter of
#' `anara::verify_ids`.
#' 
#' @param ... `metric` objects
#' @return An object of type 'metrics'
metrics <- function(...) {
  dots_list <- list(...)

  are_metrics <- map_lgl(dots_list, is_metric)
  stopifnot(all(are_metrics))

  structure(
    dots_list,
    class = "metrics"
  )
}

extract_variables <- function(x) {
  stopifnot(is_metric(x) || inherits(x, "metrics"))

  if (is_metric(x)) {
    x$var
  } else {
    unique(unlist(map(x, extract_variables)))
  }
}
