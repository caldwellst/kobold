#' Detect if question is select_multiple
#'
#' @noRd
detect_select_multiple <- function(q_name, env) {
  type <- c(filter(env$object$survey, name == q_name)$type)
  str_detect(type, "^.*(select_multiple|select multiple)")
}
