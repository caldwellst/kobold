#' Retrieve sheet name for question
#'
#' @importFrom dplyr filter
#'
#' @noRd
get_sheet <- function(q_name, env) {
  filter(env$object$survey, name == q_name)$sheet
}

#' Retrieve list name for question
#'
#' @importFrom dplyr filter
#'
#' @noRd
get_list_name <- function(q_name, env) {
  filter(env$object$survey, name == q_name)$list_name
}

#' Retrive choices list for question
#'
#' @importFrom dplyr filter
#'
#' @noRd
get_choices <- function(q_name, env) {
  l_name <- get_list_name(q_name, env)
  filter(env$object$choices, list_name == l_name)$name
}
