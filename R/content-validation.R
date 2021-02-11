validate_dat_list <- function(dat_list) {
  if (is.data.frame(dat_list) || !is.list(dat_list)) {
    stop0("`dat_list` must be a named list of data.frame objects")
  }

  if (length(dat_list) == 0) {
    stop0("No data to verify in `dat_list`")
  }

  is_df <- map_lgl(dat_list, is.data.frame)

  if (!all(is_df)) {
    stopg(c(
      "Not all elements of `dat_list` are data.frames: ",
      "{ui_vec(names(is_df[!is_df]))}"
    ))
  }
}

validate_control_column <- function(column, dat_list, err_msg) {
  has_ctrl_col <- map_lgl(dat_list, function(.x) column %in% names(.x))

  if (!all(has_ctrl_col)) {
    stopg(c(
      "Some datasets don't have {err_msg} '{id_col}': ",
      "{ui_vec(names(has_id_col[!has_id_col]))}"
    ))
  }
}

validate_uids_ <- function(dat, uid_col, db_col) {
  # Check for UID missingness
  if (any(is.na(dat[[uid_col]]))) {
    bad_dbs <- dat[is.na(dat[[uid_col]]), unique(.SD[[db_col]])]

    stopg(c(
      "These databases have missingness in record unique IDs: ",
      "{ui_vec(bad_dbs)}"
    ))
  }

  # Check for duplicate UIDs
  dat[, (".uid_isnt_unique") := any(duplicated(.SD[[uid_col]]), na.rm = TRUE), by = db_col]

  if (any(dat$.uid_isnt_unique)) {
    bad_dbs <- dat[list(TRUE), unique(.SD[[db_col]]), on = ".uid_isnt_unique"]

    stopg(c(
      "These databases have duplicated record unique IDs: ",
      "{ui_vec(bad_dbs)}"
    ))
  }

  dat[, (".uid_isnt_unique") := NULL]

  invisible(dat)
}
