#' Detect if question is select_multiple
#'
#' @importFrom dplyr filter
#' @importFrom stringr str_detect
#'
#' @noRd
detect_select_multiple <- function(q_name, env) {
  type <- c(filter(env$object$survey, name == q_name)$type)
  str_detect(type, "^.*(select_multiple|select multiple)")
}

#' Detect if question is select_one or select_multiple
#'
#' @importFrom dplyr filter
#' @importFrom stringr str_detect
#'
#' @noRd
detect_select <- function(q_name, env) {
  if (q_name %in% env$object$survey$name) {
    type <- c(filter(env$object$survey, name == q_name)$type)
    str_detect(type, "^.*(select_multiple|select multiple|select_one|select one)")
  } else {
    FALSE
  }
}
