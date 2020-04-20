#' Convert select question label responses to names
#'
#' @importFrom dplyr mutate
#' @importFrom stringr str_replace_all
#' @importFrom rlang !! sym :=
#'
#'@noRd
choice_namer <- function(q_name, sheet, language = NULL, env) {
  labels <- get_choices(q_name, type = "labels", language = language, env)
  labels <- escape_string(labels)
  labels <- pad_start_end(labels)
  names <- get_choices(q_name = q_name, env = env)
  env$object[[sheet]] <- mutate(env$object[[sheet]],
                                !!sym(q_name) := str_replace_all(!!sym(q_name),
                                                                 setNames(names, labels)))
}

#' Convert select question name responses to labels
#'
#' @importFrom dplyr mutate
#' @importFrom stringr str_replace_all
#' @importFrom rlang !! sym :=
#'
#'@noRd
choice_labeler <- function(q_name, sheet, language = NULL, env) {
  labels <- get_choices(q_name, type = "labels", language = language, env)
  names <- get_choices(q_name = q_name, env = env)
  names <- pad_start_end(names)
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
  select_cols <- map_lgl(names(env$object[[sheet]]), detect_select, env)
  select_cols <- names(env$object[[sheet]])[select_cols]
  map(select_cols, choice_namer, sheet = sheet, language = language, env = env)
}

#' Convert select question labels in dataset to choices
#'
#' @importFrom purrr map_lgl walk
#'
#' @noRd
choices_to_labels <- function(sheet, language = NULL, env) {
  select_cols <- map_lgl(names(env$object[[sheet]]), detect_select, env)
  select_cols <- names(env$object[[sheet]])[select_cols]
  map(select_cols, choice_labeler, sheet = sheet, language = language, env = env)
}

#' Convert all choice data from labels into names
#'
#' @importFrom purrr map
#' @importFrom rlang current_env
#' @importFrom dplyr pull
#'
#' @export
choice_labels_to_names <- function(object, language = NULL) {
  env <- current_env()
  map(pull(object$data_sheets, sheets), choices_to_names, language = language, env = env)
  return(object)
}

#' Convert all choice data from names into labels
#'
#' @importFrom purrr map
#' @importFrom rlang current_env
#' @importFrom dplyr pull
#'
#' @export
choice_names_to_labels <- function(object, language = NULL) {
  env <- current_env()
  map(pull(object$data_sheets, sheets), choices_to_labels, language = language, env = env)
  return(object)
}


