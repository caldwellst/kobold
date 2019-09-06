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

#' Retrive choices for question
#'
#' @importFrom dplyr filter
#' @importFrom stringr str_detect
#'
#' @noRd
get_choices <- function(q_name, type = "names", language = NULL, env) {
  l_name <- get_list_name(q_name, env)
  choices <- filter(env$object$choices, list_name == l_name)
  if (type == "names") {
    choices$name
  } else if (type == "labels") {
    if (!is.null(language)) {
      cols <- names(env$object$choices)
      col <- str_detect(cols, paste0("label[\\W]{2}(?i)", language))
      col <- cols[col]
    } else {
      col <- "label"
    }
    choices[[col]]
  }
}
