#' Escape normal string for regex
#'
#' @importFrom stringr str_replace_all
#'
#' @noRd
escape_string <- function(string) {
  str_replace_all(string, "(\\W)", "\\\\\\1")
}
