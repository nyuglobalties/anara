fix_format <- function(
  dat,
  id_col,
  unique_id_col,
  database_col,
  var_pool,
  review_fields = c("problem", "verifier", "note"),
  edit_fields = c(
    what = "what",
    change_to = "change_to",
    change_from = "change_from"
  )
) {
  stopifnot(is.data.frame(dat))

  if (data.table::is.data.table(dat)) {
    dat <- as.data.frame(dat)
  }

  dat[, review_fields] <- NA_character_
  dat[, edit_fields] <- NA_character_

  field_order <- c(
    review_fields,
    edit_fields,
    id_col,
    database_col,
    "n_issues",
    "has_duplicate",
    variable_order(dat, var_pool),
    unique_id_col
  )

  dat[, field_order]
}

variable_order <- function(dat, variables) {
  varpatterns <- glue_collapse(glue("^{variables}(_.*)?$"), "|")

  var_fields <- grep(varpatterns, names(dat), value = TRUE)
  sort(var_fields)
}
