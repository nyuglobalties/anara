#' Puts a dataset into the fix format
#' 
#' Generally only used from `anara::verify_ids`
#' 
#' @param dat The result of `anara::verify_ids`
#' @param id_col The name of the ID, or primary key, column.
#' @param unique_id_col The name of the row ID, or surrogate key,
#'   column.
#' @param database_col The name of the column that stores the database names
#' @param var_pool The pool of metric and issue columns
#' @param review_fields The names of fields to be used for verification
#' @param edit_fields The names of the fix columns
#' @return `dat` in fix format
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
