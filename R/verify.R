#' @export
verify_ids <- function(
  dat_list,
  id_col,
  unique_id_col,
  file = NULL,
  database_col = "database",
  variables = NULL,
  extra_metrics = NULL,
  verbose = TRUE,
  ...
) {
  stopifnot(is.character(id_col))
  stopifnot(is.character(unique_id_col))
  stopifnot(length(id_col) == 1 && length(unique_id_col) == 1)

  validate_dat_list(dat_list)
  validate_control_column(id_col, dat_list, "ID column")
  validate_control_column(unique_id_col, dat_list, "record unique ID column")

  var_pool <- variable_pool(dat_list, variables, extra_metrics)
  selected_data <- select_variable_pool(
    dat_list,
    id_col,
    unique_id_col,
    var_pool
  )

  selected_data <- add_database_var(selected_data, database_col)
  dat <- data.table::rbindlist(selected_data, use.names = TRUE, fill = TRUE)

  validate_uids_(dat, unique_id_col, database_col)

  dat[, ("has_duplicate") := .N > 1, by = c(id_col, database_col)]

  data.table::setkeyv(dat, id_col)
  verify_fields_(dat, variables, extra_metrics, id_col, verbose)
  add_num_issues_(dat, id_col)

  formatted_dat <- fix_format(
    dat,
    id_col,
    unique_id_col,
    database_col,
    var_pool,
    ...
  )

  if (!is.null(file)) {
    if (!dir.exists(dirname(file))) {
      if (isTRUE(verbose)) messageg("Creating new directory '{dirname(file)}'")

      dir.create(dirname(file), recursive = TRUE)
    }

    if (isTRUE(verbose)) messageg("Writing fix file '{file}'")
    data.table::fwrite(formatted_dat, file = file)
  }

  formatted_dat
}

variable_pool <- function(dat_list, variables, metrics) {
  all_vars <- variables %??% character()
  all_content_vars <- unique(unlist(map(dat_list, names)))

  if (!is.null(metrics)) {
    all_vars <- unique(c(variables, extract_variables(metrics)))
  }

  all_vars[all_vars %in% all_content_vars]
}

select_variable_pool <- function(dat_list, id_col, unique_id_col, var_pool) {
  all_vars <- c(unique_id_col, id_col, var_pool)

  map(dat_list, function(.x) {
    var_subset <- names(.x)[names(.x) %in% all_vars]

    if (data.table::is.data.table(.x)) {
      .x <- as.data.frame(.x)
    }

    .x[, var_subset]
  })
}

add_database_var <- function(dat_list, database_col) {
  for (nx in names(dat_list)) {
    dat_list[[nx]][[database_col]] <- nx
  }

  dat_list
}

verify_fields_ <- function(dat, variables, mets, id_col, verbose) {
  if (!is.null(variables)) {
    for (v in variables) {
      if (isTRUE(verbose)) messageg("Detecting default issues for '{v}'")

      if (is.numeric(dat[[v]]) && is_intlike(dat[[v]])) {
        dat[, (v) := as.integer(dat[[v]])]
      }

      dat[, paste0(v, "_issue") := detect_issues(.SD[[v]]), by = id_col]
    }
  }

  if (!is.null(mets)) {
    eval_metrics_(dat, mets, id_col)
  } else {
    if (isTRUE(verbose)) message("No extra metrics. Skipping...")
  }

  invisible(dat)
}

eval_metrics_ <- function(dat, mets, id_col) {
  for (met in mets) {
    vars <- extract_variables(met)

    for (v in vars) {
      dat[, paste0(v, "_issue_", met$tag) := met$func(.SD[[v]]), by = id_col]
    }
  }

  invisible(dat)
}

add_num_issues_ <- function(dat, id_col) {
  issue_cols <- grep("_issue|^has_duplicate$", names(dat), value = TRUE)

  if (length(issue_cols) == 0) {
    return(invisible(dat))
  }

  dat[,
    n_issues := rowSums(.SD, na.rm = TRUE),
    by = id_col,
    .SDcols = issue_cols
  ]

  invisible(dat)
}
