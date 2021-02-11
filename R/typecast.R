is_intlike <- function(x) {
  if (!is.numeric(x)) {
    return(FALSE)
  }

  x_nona <- x[!is.na(x)]

  if (length(x_nona) == 0) {
    TRUE
  } else {
    isTRUE(all.equal(rep(0, length(x_nona)), x_nona %% 1))
  }
}
