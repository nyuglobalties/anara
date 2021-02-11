detect_issues <- function(x, ...) {
  UseMethod("detect_issues", x)
}

#' @export
detect_issues.integer <- function(x, abs_distance = 1L, ...) {
  mat <- abs(outer(x, x, `-`))

  any(mat[upper.tri(mat)] > abs_distance, na.rm = TRUE)
}

#' @export
detect_issues.character <- function(x, tolerance = 0.2, ...) {
  x <- stringi::stri_trans_tolower(x)

  not_subset_mat <- not_subset_matrix(x)
  sim_mat <- stringdist::stringsimmatrix(x, x)

  not_subset_tri <- not_subset_mat[upper.tri(not_subset_mat)]
  not_sim_tri <- sim_mat[upper.tri(sim_mat)] < (1 - tolerance)

  any(not_subset_tri & not_sim_tri, na.rm = TRUE)
}

not_subset_matrix <- function(x) {
  is_subset <- outer(
    x,
    x,
    function(.x, .y) map2(.x, .y, is_subset_func)
  )

  original_dims <- dim(is_subset)
  isnt_subset <- map_lgl(is_subset, `!`)
  dim(isnt_subset) <- original_dims

  isnt_subset
}

is_subset_func <- function(elx, ely) {
  grepl(elx, ely, fixed = TRUE) | grepl(ely, elx, fixed = TRUE)
}
