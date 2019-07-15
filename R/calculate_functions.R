#' Max that defaults to remove NA
#'
#' @noRd
kobold_max <- function(x) {
  max(x, na.rm = T)
}

#' Min that defaults to remove NA
#'
#' @noRd
kobold_min <- function(x) {
  min(x, na.rm = T)
}

#' Sum that defaults to remove NA
#'
#' @noRd
kobold_sum <- function(x) {
  sum(x, na.rm = T)
}
