#' Convert select question label responses to names
#'
#' @importFrom dplyr mutate
#' @importFrom stringr str_replace_all
#' @importFrom rlang !! sym :=
#'
#'@noRd
choice_namer <- function(q_name, sheet, language = NULL, env) {
  labels <- get_choices(q_name, type = "labels", language = language, env)
  names <- get_choices(q_name, env)
  env$object[[sheet]] <- mutate(env$object[[sheet]],
                                !!sym(q_name) := str_replace_all(!!sym(q_name),
                                                                 setNames(labels, names)))
}

#' Convert select question labels in dataset to choices
#'
#' @importFrom purrr map_lgl walk
#'
#' @noRd
choices_to_names <- function(sheet, language = NULL, env) {
  select_cols <- map_lgl(names(env$obj[[sheet]]), detect_select, env)
  select_cols <- names(env$obj[[sheet]])[select_cols]
  walk(select_cols, choice_namer, sheet = sheet, language = language, env = env)
}

#' Convert all choice data from labels into names
#'
#' @importFrom purrr walk
#'
#' @noRd
choice_labels_to_names <- function(object, language = NULL) {
  env <- current_env()
  walk(object$data_sheets, choices_to_names, language = language, env)
}
