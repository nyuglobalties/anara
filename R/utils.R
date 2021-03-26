`%??%` <- function(x, y) if (is.null(x)) y else x

stop0 <- function(...) {
  stop(..., call. = FALSE)
}

warn0 <- function(...) {
  warning(..., call. = FALSE, immediate. = TRUE)
}

stopg <- function(x, .env = parent.frame()) {
  stop0(glue(glue_collapse(x), .envir = .env))
}

messageg <- function(x, .env = parent.frame()) {
  message(glue(glue_collapse(x), .envir = .env))
}

tk_err <- function(x, .envir = parent.frame()) {
  msg <- glue(glue_collapse(x), .envir = .envir)

  rlang::abort(.subclass = "tk_error", message = msg)
}

tk_warn <- function(x, .envir = parent.frame()) {
  msg <- glue(glue_collapse(x), .envir = .envir)

  rlang::warn(.subclass = "tk_warning", message = msg)
}

tk_assert <- function(x, msg = NULL, .envir = parent.frame()) {
  if (is.null(msg)) {
    deparsed <- deparse(substitute(x))
    msg <- glue("Assertion {ui_quote(deparsed)} not met")
  } else {
    msg <- glue(glue_collapse(msg, "\n"), .envir = .envir)
  }

  if (!isTRUE(x)) {
    tk_err(msg)
  }

  invisible()
}

ui_vec <- function(x, max_len = 10) {
  if (is.character(x)) {
    chr_x <- glue("'{x}'")
  } else {
    chr_x <- as.character(x)
  }

  if (max_len > 0 && length(x) > max_len) {
    chr_x <- chr_x[1:(max_len + 1)]
    chr_x[max_len + 1] <- "..."
  }

  glue("[{glue_collapse(chr_x, ', ')}]")
}

cat_line <- function(x = NULL, .env = parent.frame()) {
  cat(glue(glue_collapse(x), .envir = .env), "\n", sep = "")
}

get_attr <- function(obj, attrib) {
  attr(obj, attrib, exact = TRUE)
}

set_attrs <- function(obj, ...) {
  dots <- rlang::dots_list(...)

  if (is.null(names(dots)) || any(names(dots) == "")) {
    stop0("All attribs must have names")
  }

  for (d in names(dots)) {
    attr(obj, d) <- dots[[d]]
  }

  obj
}
