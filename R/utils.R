`%??%` <- function(x, y) if (is.null(x)) y else x

stop0 <- function(...) {
  stop(..., call. = FALSE)
}

stopg <- function(x, .env = parent.frame()) {
  stop0(glue(glue_collapse(x), .envir = .env))
}

messageg <- function(x, .env = parent.frame()) {
  message(glue(glue_collapse(x), .envir = .env))
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
