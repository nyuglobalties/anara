VERIFIED_FIXES_ATTR <- "verified_fixes"

validate_foreign_keys <- function(fixes, context) {
  DB <- context$database
  dbname <- context$database_name
  foreign_keys <- context$foreign_keys

  present_keys <- foreign_keys[foreign_keys %in% names(DB)]
}

validate_key_structure <- function(
  databases,
  reference,
  primary_key,
  foreign_keys,
  database_names = NULL
) {
  tk_assert(is.data.frame(reference) || is.character(reference))

  working_dbs <- validate_db_struct(databases, database_names)

  if (is.character(reference)) {
    reference <- working_dbs[[reference]]
  }

  db_key_sets <- vector("list", length(working_dbs))

  for (nwdb in seq_along(working_dbs)) {
    db_key_sets[[nwdb]] <- database_key_set(
      database = working_dbs[[nwdb]],
      pk = primary_key,
      fks = foreign_keys,
      database_name = names(working_dbs)[nwdb]
    )
  }
}

validate_db_struct <- function(databases, database_names) {
  tk_assert(is.data.frame(databases) || is.list(databases))
  tk_assert(is.character(database_names) || is.null(database_names))

  missing_err <- c(
    "Database names must be provided, either with `database_names` ",
    "or with the names of the input list."
  )

  no_duplicates_err <- "Databases must have unique identifiers"
  no_na_err <- "Databases must have unique identifiers"
  num_databases <- 1

  if (!is.data.frame(databases)) {
    if (is.list(databases)) {
      if (!all(vlapply(databases, is.data.frame))) {
        tk_err("List of databases must be a `list` of data.frames")
      }

      num_databases <- length(databases)

      if (is.null(database_names) && is.null(names(databases))) {
        tk_err(missing_err)
      } else if (is.null(database_names) && !is.null(names(databases))) {
        database_names <- names(databases)
      }
    }
  }

  tk_assert(length(database_names) == num_databases)

  if (any(vlapply(database_names, duplicated))) {
    tk_err(no_duplicates_err)
  }

  if (any(vlapply(database_names, is.na))) {
    tk_err(no_na_err)
  }

  if (is.data.frame(databases)) {
    outlist <- vector("list", 1)
    outlist[[1]] <- databases
    names(outlist) <- database_names
    return(outlist)
  } else {
    names(databases) <- database_names
    return(databases)
  }
}

database_key_set <- function(database, pk, fks, reference = FALSE, database_name = NULL) {
  tk_assert(is.data.frame(database))
  tk_assert(is.character(pk))
  tk_assert(is.character(fks))

  if (!data.table::is.data.table(database)) {
    data.table::setDT(database)
  }

  if (!pk %in% names(database)) {
    tk_err("Primary key {ui_value(pk)} not found in database {if (!is.null(database_name)) ui_value(database_name)}")
  }

  not_present_fks <- fks[!fks %in% names(database)]

  if (length(not_present_fks) > 0) {
    if (isTRUE(reference)) {
      tk_warn("[{glue_collapse(ui_value(not_present_fks), ', ')}] not found in reference database")
    }

    fks <- setdiff(fks, not_present_fks)
  }

  database[, c(pk, fks), with = FALSE]
}

#' Verifies the prospective fixes
#' 
#' Computes metrics to determine if the requested fixes are valid
#' and won't cause record-level corruption. These fixes *don't*
#' perform referential integrity checks. That must be done externally.
#' 
#' @param fixes A `data.frame` in the fix format
#' @param id_col The name of the column that contains the primary key
#' @param unique_id_col The name of the column that contains the surrogate key
#' @param databases A list of `data.frames` used to validate the fixes
#' @param reference A master `data.frame` that contains the "ground truth"
#'   of the information found in the `databases`
#' @param foreign_keys Not used.
#' @param fix_history A previous iteration of fixes when, if provided, will
#'   be used to determine if fixes found in `fixes` are amendments of
#'   previous fixes.
#' @param include_problem_cases If `TRUE`, records marked as a "problem",
#'   which is an internal communication column for further review, will be
#'   included in the fix verification metrics.
#' @param verbose Enables logging messages
#' @param review_fields The names of fields to be used for verification
#' @param edit_fields The names of the fix columns
#' @return A `data.frame` of fixes with the "verified_fixes" attribute,
#'   along with the fix verification metrics.
#' 
#' @export 
verify_fixes <- function(
  fixes,
  id_col,
  unique_id_col = "unique_id",
  databases = NULL,
  reference = NULL,
  foreign_keys = NULL,
  fix_history = NULL,
  include_problem_cases = TRUE,
  review_fields = c("problem", "verifier", "note"),
  edit_fields = c(
    what = "what",
    change_to = "change_to",
    change_from = "change_from"
  ),
  verbose = TRUE
) {
  if (!inherits(fixes, "data.frame")) {
    if (is.list(fixes)) {
      if (all(vlapply(fixes, inherits, "data.frame"))) {
        fixes <- data.table::rbindlist(fixes, use.names = TRUE, fill = TRUE)
      } else {
        stop0("fixes must either inherit from a data.frame OR be a list of data.frames")
      }
    } else {
      stop0("fixes must either inherit from a data.frame OR be a list of data.frames")
    }
  }

  fixes <- validate_fix_cols(
    fixes, 
    unique_id_col, 
    id_col,
    review_fields,
    edit_fields
  )

  fixes[, missing_uid := FALSE]
  fixes[, duplicate_changes := FALSE]
  fixes[, multiple_conclusions := FALSE]
  fixes[, what_not_found := FALSE]
  fixes[, bad_change_from := FALSE]
  fixes[, conflicting_id := ""]
  fixes[, existing_id := FALSE]
  fixes[, nonexistent_id_removed := FALSE]
  fixes[, uid_count := .N, by = c("database", "unique_id")]
  fixes[, identical_request := grepl("^identical$", what, ignore.case = TRUE)]
  fixes[, delete_request := grepl("^whole obs", what, ignore.case = TRUE)]

  fixes[, incomplete_record := FALSE]
  fixes[, empty_check := apply(.SD, 1L, function(x) !all(is.na(x) | grepl("^\\s*$", x)) & any(is.na(x) | grepl("^\\s*$", x))), .SDcols = c("what", "change_from", "change_to")]
  fixes[empty_check == TRUE, incomplete_record := TRUE]
  fixes[empty_check == TRUE & (identical_request == TRUE | delete_request == TRUE), incomplete_record := FALSE]
  fixes[, empty_check := NULL]

  if ("problem" %in% names(fixes)) {
    fixes[, problem_case := grepl("yes", problem, ignore.case = TRUE)]
  } else {
    fixes[, problem_case := FALSE]
  }

  fixes[, change_to := as.character(change_to)]

  fixes[uid_count > 1L, duplicate_changes := any(duplicated(fixhash)), by = c("database", "unique_id")]
  fixes[duplicate_changes == TRUE, multiple_conclusions := length(unique(change_to)) > 1L, by = c("database", "unique_id")]

  # Bugfix 2019-09-25: If the same UID had multiple changes but any ONE of them is either an identical or delete request,
  # raise the multiple_conclusions flag
  fixes[uid_count > 1L, multiple_conclusions := multiple_conclusions | any(delete_request == TRUE | identical_request == TRUE), by = c("database", "unique_id")]

  if (!is.null(databases)) {
    # Ensure that a list is passed in, not a data.frame
    stopifnot(!inherits(databases, "data.frame") && is.list(databases) && !is.null(names(databases)))

    for (dbname in names(databases)) {
      if (!dbname %in% fixes[, unique(database)]) {
        warn0("Database ", dbname, " not found in fixes.")
        next
      }

      DB <- data.table::as.data.table(databases[[dbname]])

      query <- bquote(database == dbname & !.(as.name(unique_id_col)) %in% DB[[unique_id_col]])
      fixes[eval(query), missing_uid := TRUE]
      fixes[database == dbname & (!(is.na(what) | grepl("^\\s*$|^whole obs|^identical$", what, ignore.case = TRUE))), what_not_found := !what %in% names(DB)]

      db_id_col <- if (!is.list(id_col)) {
        id_col
      } else {
        if (is.null(names(id_col))) {
          stop0("If a list() is provided for id_col, it must have names where each name is a database name or '.others'")
        }

        if (dbname %in% names(id_col)) {
          id_col[[dbname]]
        } else if (".others" %in% names(id_col)) {
          id_col[[".others"]]
        } else {
          stopg("Database '{dbname}' not found in id_col")
        }
      }

      idcolsym <- as.name(db_id_col)
      query <- bquote(.(idcolsym) := as.character(.(idcolsym)))
      DB[, eval(query)]

      q1 <- bquote(DB[, unique(.(idcolsym))])
      id_pool <- DB[, .(Count = .N), by = db_id_col]
      data.table::setnames(id_pool, db_id_col, "entity_id")

      fixes[database == dbname, id_change := what == ..db_id_col]

      if (!is.null(reference)) {
        stopifnot(inherits(reference, "data.frame"))
        fixes[database == dbname & id_change == TRUE, unknown_changed_id := !change_to %in% reference[[db_id_col]] & !grepl("^unassigned|^unidentified", change_to, ignore.case = TRUE)]
      }

      id_change_from <- fixes[database == dbname & missing_uid == FALSE & id_change == TRUE & incomplete_record == FALSE & change_from != "NULL", .(entity_id = change_from, Count = -1)] # Don't count NULL, the signifier that something was missing

      del_record <- fixes[database == dbname & missing_uid == FALSE & delete_request == TRUE, list(entity_id = id, Count = -1)]

      id_change_to <- fixes[database == dbname & missing_uid == FALSE & id_change == TRUE & incomplete_record == FALSE, .(entity_id = change_to, Count = 1)]

      id_pool <- data.table::rbindlist(list(id_pool, id_change_from, id_change_to, del_record), use.names = TRUE)
      id_pool <- id_pool[, .(Count = sum(Count)), by = entity_id]

      existing_ids <- id_pool[Count > 1L, entity_id]
      removed_nonexistent_ids <- id_pool[Count < 0L, entity_id]

      fixes[database == dbname & change_to %in% existing_ids, existing_id := TRUE]
      fixes[database == dbname & change_to %in% existing_ids, conflicting_id := as.character(change_to)]

      fixes[database == dbname & change_from %in% removed_nonexistent_ids, nonexistent_id_removed := TRUE]
      fixes[database == dbname & change_from %in% removed_nonexistent_ids, conflicting_id := as.character(change_from)]
    }
  }

  err_cols <- c(
    "missing_uid",
    "incomplete_record",
    "duplicate_changes",
    "multiple_conclusions",
    "what_not_found",
    "unknown_changed_id",
    "existing_id",
    "nonexistent_id_removed",
    "problem_case"
  )

  if (!isTRUE(include_problem_cases)) {
    err_cols <- err_cols[-length(err_cols)]
  }

  if (!"unknown_changed_id" %in% names(fixes)) {
    err_cols <- setdiff(err_cols, "unknown_changed_id")
  }

  # Unknown changed ID should really only be a bookkeeping measure, not necessarily a bad thing. Do not treat as "issue".
  fixes[, state := {
    any_issue <- apply(.SD, 1L, any, na.rm = TRUE)

    ifelse(any_issue == TRUE, "rejected", "accepted")
  }, .SDcols = err_cols[!grepl("unknown_changed_id", err_cols)]]

  if (!is.null(fix_history)) {
    stopifnot(inherits(fix_history, "data.frame"))
    fix_history <- data.table::as.data.table(fix_history)

    # Make a marker to indicate which part is what
    fix_history[, historic_fixes := TRUE]
    fixes[, historic_fixes := FALSE]

    fixes[, record_signature := apply(.SD, 1L, digest::digest, algo = "xxhash64"), .SDcols = c("unique_id", "database")]

    total_fixes <- data.table::rbindlist(list(fix_history, fixes), use.names = TRUE, fill = TRUE)

    total_fixes[, previous_modification := vlapply(fixhash, function(x) x %in% reversehash)]
    total_fixes[previous_modification == TRUE, old_modification := vcapply(fixhash, function(x) {
      total_fixes[reversehash == x, fixhash]
    })]

    total_fixes[, deleted_later := FALSE]
    total_fixes[duplicated(record_signature) | duplicated(record_signature, fromLast = TRUE), deleted_later := {
      any(historic_fixes == TRUE) &&
        any(historic_fixes == FALSE) &&
        .SD[historic_fixes == FALSE, any(delete_request == TRUE)]
    }, by = record_signature]
    total_fixes[, previous_modification := previous_modification | deleted_later]

    total_fixes[, repeated_modification := FALSE]
    total_fixes[duplicated(fixhash) | duplicated(fixhash, fromLast = TRUE), repeated_modification := {
      any(historic_fixes == TRUE) && any(historic_fixes == FALSE)
    }, by = record_signature]

    total_fixes[state == "accepted" & previous_modification == TRUE, state := "amended"]
    total_fixes[repeated_modification == TRUE, state := "rejected"]

    fixes <- total_fixes[historic_fixes == FALSE]
    fixes[, historic_fixes := NULL]
    fixes[, record_signature := NULL]
  }

  attr(fixes, VERIFIED_FIXES_ATTR) <- TRUE

  fixes
}

validate_fix_cols <- function(
  df, 
  unique_id_col, 
  id_col, 
  review_fields,
  edit_fields
) {
  if (!unique_id_col %in% names(df)) {
    stopg("Unique ID column '{unique_id_col}' not found in fixes")
  }

  if (!id_col %in% names(df)) {
    stopg("ID column '{id_col}' not found in fixes")
  }

  for (nfield in names(edit_fields)) {
    if (!edit_fields[[nfield]] %in% names(df)) {
      stopg("Edit field '{edit_fields[[nfield]]}' not found in fixes")
    }
  }

  for (nfield in names(review_fields)) {
    if (!review_fields[[nfield]] %in% names(df)) {
      warng("Review field '{review_fields[[nfield]]}' not found in fixes")
    }
  }

  df <- data.table::as.data.table(df)
  data.table::setnames(df, edit_fields, names(edit_fields))

  df[, fixhash := apply(.SD, 1L, digest::digest, algo = "xxhash64"), .SDcols = c("database", unique_id_col, "what", "change_from")]
  df[, reversehash := apply(.SD, 1L, digest::digest, algo = "xxhash64"), .SDcols = c("database", unique_id_col, "what", "change_to")]

  df
}

integrity_report <- function(verified_fixes, file = NULL, include_problem_cases = TRUE) {
  stopifnot(inherits(verified_fixes, "data.frame"))

  if (!isTRUE(attr(verified_fixes, VERIFIED_FIXES_ATTR))) {
    stop("`integrity_report()` only accepts data.frames that have been through `verify_fixes()`", call. = FALSE)
  }

  if (!data.table::is.data.table(verified_fixes)) {
    verified_fixes <- data.table::as.data.table(verified_fixes)
  }

  err_cols <- c(
    "missing_uid",
    "incomplete_record",
    "duplicate_changes",
    "multiple_conclusions",
    "what_not_found",
    "unknown_changed_id",
    "existing_id",
    "nonexistent_id_removed",
    "problem_case"
  )

  if (!"unknown_changed_id" %in% names(verified_fixes)) {
    err_cols <- setdiff(err_cols, "unknown_changed_id")
  }

  cols <- if (isTRUE(include_problem_cases) && any(verified_fixes[, problem_case])) {
    c("database", "unique_id", "conflicting_id", err_cols)
  } else {
    c("database", "unique_id", "conflicting_id", setdiff(err_cols, "problem_case"))
  }

  issues <- verified_fixes[state == "rejected", ..cols]

  if (!is.null(file)) {
    if (nrow(issues) > 0L) {
      data.table::fwrite(issues, file = file)
    } else {
      if (file.exists(file)) {
        message("Deleting old issues for database with no issues")
        unlink(file)
      }
    }
  }

  verified_fixes
}

fix_application_diagnostics <- function(
  verified_fixes,
  databases,
  unique_id_col,
  verbose = FALSE
) {
  stopifnot(inherits(verified_fixes, "data.frame"))

  if (!isTRUE(attr(verified_fixes, VERIFIED_FIXES_ATTR))) {
    stop("`integrity_report()` only accepts data.frames that have been through `verify_fixes()`", call. = FALSE)
  }

  if (!data.table::is.data.table(verified_fixes)) {
    verified_fixes <- data.table::as.data.table(verified_fixes)
  }

  # Only look at accepted or amended fixes -- and temporarily only check variable changes
  verified_fixes <- verified_fixes[state != "rejected" & !(identical_request == TRUE | delete_request == TRUE)]
  verified_fixes[, applied := NA]

  uids <- if (!is.list(unique_id_col)) {
    .uids <- lapply(names(databases), function(x) unique_id_col)
    names(.uids) <- names(databases)

    .uids
  } else {
    for (n in names(databases)) {
      if (!n %in% names(unique_id_col) && ".others" %in% names(unique_id_col)) {
        unique_id_col[[n]] <- unique_id_col[[".others"]]
      } else if (!n %in% names(unique_id_col) && !".others" %in% names(unique_id_col)) {
        stop("Undefined unique ID column for database ", n, call. = FALSE)
      }
    }

    if (".others" %in% unique_id_col) {
      unique_id_col[[".others"]] <- NULL
    }

    unique_id_col
  }

  for (ndat in names(databases)) {
    DB <- as.data.table(databases[[ndat]])

    for (fh in verified_fixes[database == ndat, fixhash]) {
      verified_fixes[fixhash == fh, applied := identical(verified_fixes[fixhash == fh, as.character(change_to)], as.character(DB[DB[[uids[[ndat]]]] == verified_fixes[fixhash == fh, unique_id], verified_fixes[fixhash == fh, WHAT], with = FALSE]))]
    }
  }

  verified_fixes
}
