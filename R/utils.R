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

anara_err <- function(x, .envir = parent.frame()) {
  msg <- glue(glue_collapse(x), .envir = .envir)

  stop0(errorCondition(
    message = msg,
    class = "anara_error"
  ))
}

anara_warn <- function(x, .envir = parent.frame()) {
  msg <- glue(glue_collapse(x), .envir = .envir)

  warn0(warningCondition(
    message = msg,
    class = "anara_warning"
  ))
}

anara_assert <- function(x, msg = NULL, .envir = parent.frame()) {
  if (is.null(msg)) {
    deparsed <- deparse(substitute(x))
    msg <- glue("Assertion {ui_quote(deparsed)} not met")
  } else {
    msg <- glue(glue_collapse(msg, "\n"), .envir = .envir)
  }

  if (!isTRUE(x)) {
    anara_err(msg)
  }

  invisible(x)
}

vapply_mold <- function(type) {
  function(.x, .f, ...) {
    vapply(.x, .f, type, ...)
  }
}

vlapply <- vapply_mold(logical(1))
viapply <- vapply_mold(integer(1))
vdapply <- vapply_mold(double(1))
vcapply <- vapply_mold(character(1))

lapply2 <- function(.x, .y, .f, ...) {
  out <- mapply(.f, .x, .y, MoreArgs = list(...), SIMPLIFY = FALSE)
  if (length(out) == length(.x)) {
    stats::setNames(out, names(.x))
  } else {
    stats::setNames(out, NULL)
  }
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
  dots <- list(...)

  if (is.null(names(dots)) || any(names(dots) == "")) {
    stop0("All attribs must have names")
  }

  for (d in names(dots)) {
    attr(obj, d) <- dots[[d]]
  }

  obj
}
