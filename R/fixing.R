#' Apply fixes to databases
#' 
#' @param databases A named list of `data.frame` objects The list names correspond
#'   to the entry in the `database` column of the fix file.
#' @param fixes A `data.frame` containing the *verified* fixes. Must have
#'   been through `anara::verify_fixes`.
#' @param unique_id_col The column name for the column that is the surrogate
#'   key in all `databases`
#' @param id_col The column name for the column that is the primary key
#'   in all `databases`
#' @param verbose Produces logging messages if `TRUE`
#' @param force_fix If `TRUE`, applies fixes identified as problematic in 
#'   `anara::verify_fixes`. Generally an unsafe operation!
#' @return A list of corrected `data.frames`
#' 
#' @export
apply_fixes <- function(
  databases,
  fixes,
  unique_id_col,
  id_col,
  verbose = FALSE,
  force_fix = FALSE
) {
  correct_multiple(
    fixes,
    databases,
    unique_id_col,
    id_col,
    verbose = verbose,
    force_fix = force_fix
  )
}

correct_single <- function(corrections, database, unique_id_col, id_col, verbose = FALSE, force_fix = FALSE) {
  log_db <- function(..., .level = "info") {
    if (isTRUE(verbose)) cat(..., "\n", sep = "", file = if (identical(.level, "info")) stdout() else stderr())

    invisible(NULL)
  }

  if (!inherits(database, "data.frame")) {
    stop0("`database` must be a data.frame or similar object (e.g. a tbl_df or data.table).")
  }

  if (is.character(corrections)) {
    log_db("Loading corrections from path")

    # Treat as a path
    stopifnot(length(corrections) == 1L)

    if (!file.exists(corrections)) {
      stop0("`corrections` file not found: ", corrections)
    }

    corrections <- if (grepl("\\.csv$", corrections)) {
      read.csv(corrections, stringsAsFactors = FALSE, na.strings = c("", "NA"))
    } else if (grepl("\\.xlsx", corrections)) {
      openxlsx::read.xlsx(corrections, na.strings = c("", "NA"))
    } else {
      NULL
    }

    if (is.null(corrections)) {
      stop0("Could not read `corrections` file")
    }

    log_db("Successfully loaded corrections")
  } else {
    if (!inherits(corrections, "data.frame")) {
      stop0("`corrections` must be a data.frame or similar object OR a path to the corrections file")
    }
  }

  DT <- data.table::as.data.table(database)
  CORRECT <- data.table::as.data.table(corrections)

  if (!isTRUE(get_attr(CORRECT, VERIFIED_FIXES_ATTR))) {
    stop0("Please run `anara::verify_fixes` on your fixes first")
  }

  # Assert that the change.to column has character data
  CORRECT[, change_to := as.character(change_to)]

  # Assert that the ID column in the database has character data
  query <- bquote(.(as.name(id_col)) := as.character(.(as.name(id_col))))
  DT[, eval(query)]

  CORRECT <- CORRECT[!is.na(what)]

  ACCEPTED <- if (!isTRUE(force_fix)) {
    if ("state" %in% names(CORRECT)) {
      CORRECT[state != "rejected"]
    } else {
      CORRECT[any_issue == FALSE]
    }
  } else {
    CORRECT[]
  }

  # Records marked to delete have "Whole Observation" in WHAT
  deletions <- ACCEPTED[grepl("^whole obs", what, ignore.case = TRUE), unique_id]
  del_hashes <- ACCEPTED[grepl("^whole obs", what, ignore.case = TRUE), fixhash]
  query <- bquote(!.(as.name(unique_id_col)) %in% deletions)

  for (i in seq_along(deletions)) {
    log_db("(", del_hashes[i], ") ", deletions[i], " deleted")
  }

  DT <- DT[eval(query)]

  # Identical cases are those where the recorded data is approximately identical. In this situation,
  # randomly select which record to keep, grouped by ID
  if (nrow(ACCEPTED[grepl("^identical$", what, ignore.case = TRUE)]) > 0L) {
    reject_rows <- ACCEPTED[grepl("^identical$", what, ignore.case = TRUE), .(.rows = sample(.I, .N - 1)), by = id][, .rows]
    reject_uids <- ACCEPTED[reject_rows, unique_id]
    reject_hashes <- ACCEPTED[reject_rows, fixhash]

    for (i in seq_along(reject_uids)) {
      log_db("(", reject_hashes[i], ") ", reject_uids[i], " identical dropped")
    }

    query <- bquote(!.(as.name(unique_id_col)) %in% reject_uids)
    DT <- DT[eval(query)]
  }

  other_vars <- ACCEPTED[!grepl("^whole obs|^identical$", what, ignore.case = TRUE), unique(what)]

  for (ov in other_vars) {
    ovhashes <- ACCEPTED[what == ov, fixhash]
    ovsym <- as.name(ov)

    type_query <- bquote(typeof(.(ovsym)))

    if (ACCEPTED[, typeof(change_to)] != DT[, eval(type_query)]) {
      message("Converted ", ov, " to a character vector. Please compensate for this!")
      query <- bquote(.(ovsym) := as.character(.(ovsym)))
      DT[, eval(query)]
    }

    for (ovhash in ovhashes) {
      ovuid <- ACCEPTED[fixhash == ovhash, unique_id]

      q_filter <- bquote(.(as.name(unique_id_col)) == ovuid)
      q_change <- bquote(.(ovsym) := ACCEPTED[fixhash == ovhash, change_to])

      current <- DT[eval(q_filter)][[ov]]
      change <- ACCEPTED[fixhash == ovhash, change_to]

      log_db("(", ovhash, ") ", ovuid, " @ ", ov, ": ", current, " -> ", change)
      DT[eval(q_filter), eval(q_change)]
    }
  }

  # Send the list of bad IDs to console for now
  if ("state" %in% names(CORRECT)) {
    CORRECT[, any_issue := state == "rejected"]
  }

  if (nrow(CORRECT[any_issue == TRUE]) > 0L) {
    if (isTRUE(force_fix)) {
      warn0("Bad fixes were applied! Look out for these.")
    }

    print("The following are bad fix requests:")
    print(CORRECT[any_issue == TRUE])
  }

  as.data.frame(DT)
}

correct_multiple <- function(corrections, databases, unique_id_col, id_col, verbose = FALSE, force_fix = FALSE) {
  log_db <- function(..., .level = "info") {
    if (isTRUE(verbose)) cat(..., "\n", sep = "", file = if (identical(.level, "info")) stdout() else stderr())

    invisible(NULL)
  }

  stopifnot(!inherits(databases, "data.frame"), is.list(databases), !is.null(names(databases)))
  stopifnot(isTRUE(attr(corrections, VERIFIED_FIXES_ATTR)))

  uids <- if (!is.list(unique_id_col)) {
    .uids <- lapply(names(databases), function(x) unique_id_col)
    names(.uids) <- names(databases)

    .uids
  } else {
    for (n in names(databases)) {
      if (!n %in% names(unique_id_col) && ".others" %in% names(unique_id_col)) {
        unique_id_col[[n]] <- unique_id_col[[".others"]]
      } else if (!n %in% names(unique_id_col) && !".others" %in% names(unique_id_col)) {
        stop0("Undefined unique ID column for database ", n)
      }
    }

    if (".others" %in% unique_id_col) {
      unique_id_col[[".others"]] <- NULL
    }

    unique_id_col
  }

  ids <- if (!is.list(id_col)) {
    .ids <- lapply(names(databases), function(x) id_col)
    names(.ids) <- names(databases)

    .ids
  } else {
    for (n in names(databases)) {
      if (!n %in% names(id_col) && ".others" %in% names(id_col)) {
        id_col[[n]] <- id_col[[".others"]]
      } else if (!n %in% names(id_col) && !".others" %in% names(id_col)) {
        stop0("Undefined ID column for database ", n)
      }
    }

    if (".others" %in% id_col) {
      id_col[[".others"]] <- NULL
    }

    id_col
  }

  out <- lapply(names(databases), function(ndat) {
    correct <- corrections[database == ndat]

    log_db("Fixing issues in '", ndat, "' ...")
    correct_single(
      corrections = correct,
      database = databases[[ndat]],
      unique_id_col = uids[[ndat]],
      id_col = ids[[ndat]],
      verbose = verbose,
      force_fix = force_fix
    )
  })

  names(out) <- names(databases)

  out
}
