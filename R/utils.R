#' Escape normal string for regex
#'
#' @importFrom stringr str_replace_all
#'
#' @noRd
escape_string <- function(string) {
  str_replace_all(string, "(\\W)", "\\\\\\1")
}

#' Get argument names from ellipsis
#'
#' @importFrom purrr map_chr
#'
#' @noRd
ellipsis_names <- function(...) {
  args <- as.list(substitute(list(...)))[-1L]
  map_chr(args, deparse)
}
