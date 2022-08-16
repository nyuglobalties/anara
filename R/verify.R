#' Verify record consistency across databases
#'
#' Compares demographic information across datasets to determine
#' if the entity identified with ID `x` is the same across all
#' datasets.
#'
#' @param dat_list A named list of `data.frames`
#' @param id_col The name of the ID, or primary key, column.
#'   For consistency, should be the same across datasets.
#' @param unique_id_col The name of the row ID, or surrogate key,
#'   column. For consistency, should be the same across datasets.
#' @param file If not `NULL`, a path to where the output spreadsheet
#'   will be saved.
#' @param database_col The column name to store the `dat_list` names
#' @param variables A character vector of integer or character columns
#'   to be used for comparison across datasets.
#' @param tolerances If not `NULL`, a `list` of parameters to be used as tolerances.
#'   The list names must be variable names provided to `variables`, and the type
#'   of tolerances depends on the variable:
#'   * If the variable is an integer, the tolerance is the maximum difference allowed
#'   * If the variable is a character, the tolerance is maximum dissimilarity allowed,
#'     measured between 0 and 1.
#' @param extra_metrics A `metrics()` call that contains a collection of
#'   `metric()` calls
#' @param extra_cols A character vector of columns to be included in the output
#'   verification spreadsheet, mainly for reference and support during
#'   manual inspection
#' @param verbose Enables logging
#' @param ... Extra parameters passed to `anara::fix_format`
#' @return A `data.frame` in the fix format
#' @export
#'
#' @examples
#' if (FALSE) {
#'   anara::verify_ids(
#'     list(
#'       database1 = dat_1,
#'       database2 = dat_2
#'     ),
#'     id_col = "participant_id",
#'     unique_id_col = "unique_id",
#'     variables = c("female", "grade", "teacher_name", "form"),
#'     tolerances = list(
#'       form = 0,
#'       teacher_name = 0.05
#'     ),
#'     extra_cols = c(
#'       "start", "end",
#'       "incdnt_01", "incdnt_01_o", "incdnt_02", "incdnt_02_o"
#'     ),
#'     file = file.path("path", "to", "issues.csv")
#'   )
#' }
verify_ids <- function(dat_list,
                       id_col,
                       unique_id_col,
                       file = NULL,
                       database_col = "database",
                       variables = NULL,
                       tolerances = NULL,
                       extra_metrics = NULL,
                       extra_cols = NULL,
                       verbose = TRUE,
                       ...) {
  stopifnot(is.character(id_col))
  stopifnot(is.character(unique_id_col))
  stopifnot(length(id_col) == 1 && length(unique_id_col) == 1)
  stopifnot(is.null(tolerances) || is.list(tolerances))
  stopifnot(is.null(extra_cols) || is.character(extra_cols))

  validate_dat_list(dat_list)
  validate_control_column(id_col, dat_list, "ID column")
  validate_control_column(unique_id_col, dat_list, "record unique ID column")

  var_pool <- variable_pool(dat_list, c(variables, extra_cols), extra_metrics)
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
  verify_fields_(dat, variables, tolerances, extra_metrics, id_col, verbose)
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
  all_content_vars <- unique(unlist(lapply(dat_list, names)))

  if (!is.null(metrics)) {
    all_vars <- unique(c(variables, extract_variables(metrics)))
  }

  all_vars[all_vars %in% all_content_vars]
}

select_variable_pool <- function(dat_list, id_col, unique_id_col, var_pool) {
  all_vars <- c(unique_id_col, id_col, var_pool)

  lapply(dat_list, function(.x) {
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

verify_fields_ <- function(dat, variables, tols, mets, id_col, verbose) {
  if (!is.null(variables)) {
    for (v in variables) {
      if (isTRUE(verbose)) messageg("Detecting default issues for '{v}'")

      if (is.numeric(dat[[v]]) && is_intlike(dat[[v]])) {
        dat[, (v) := as.integer(dat[[v]])]
      }

      if (!is.null(tols[[v]])) {
        dat[, paste0(v, "_issue") := detect_issues(.SD[[v]], tol = tols[[v]]), by = id_col]
      } else {
        dat[, paste0(v, "_issue") := detect_issues(.SD[[v]]), by = id_col]
      }
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
