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
