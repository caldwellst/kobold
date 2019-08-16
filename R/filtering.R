#' Generate filter expression
#'
#' @importFrom glue glue
#' @importFrom rlang parse_expr
#'
filter_expr_generator <- function(var_index = NA, var_uuid = NA, relevant = NA) {
  if (!is.na(var_index)) {
    parse_expr("index %in% var_index")
  } else if (!is.na(var_uuid)) {
    parse_expr("uuid %in% var_uuid")
  } else if (!is.na(relevant)) {
    convert_relevant(relevant)
  }
}

#' Retrieve object name as character
#'
#' @noRd
get_name <- function(x) {
  deparse(substitute(x))
}
