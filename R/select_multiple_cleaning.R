#' Remove select_multiple value
#'
#' @importFrom glue glue
#' @importFrom stringr str_trim str_replace str_remove str_c
#'
#' @noRd
sm_str_remover <- function(column, value) {
  option = glue("\\b{value}\\b")
  str_trim(str_replace(str_remove(column, option), "  ", " "))
}

#' Add select_multiple value
#'
#' @importFrom glue glue
#' @importFrom stringr str_split str_c
#' @importFrom purrr map map_chr map_lgl
#'
#' @noRd
sm_str_adder <- function(column, value, choices) {
  split <- str_split(column, pattern = " ")
  exist_test <- map_lgl(split, function(x)
    is.na(match(value, x)))
  split <- ifelse(exist_test, map(split, append, value), split)
  split <- map(split, function(x)
    x[order(match(x, choices))])
  map_chr(split, function(x) str_c(x[!is.na(x)], collapse = " "))
}

#' Change binary columns for select_multiple data
#'
#' @importFrom dplyr filter select matches mutate_at
#' @importFrom glue glue glue_collapse
#' @importFrom tidyr replace_na
#' @importFrom rlang !! sym
#'
#' @noRd
sm_na_replacer <- function(q_name, value, filter_expr, var_uuid, var_index, relevant, sheet, choices, env) {
  cols <- get_binary_cols(q_name, sheet, choices, env)
  binaries <- filter(env$object[[sheet]], !!filter_expr) %>% select(cols)
  if (anyNA(binaries)) {
    env$object[[sheet]] <- mutate_at(env$object[[sheet]], cols,
                                     ~ ifelse(!!filter_expr, replace_na(.x, 0), .x))
  }
}

#' Change binary columns for select_multiple data
#'
#' @importFrom dplyr filter select matches mutate_at
#' @importFrom glue glue glue_collapse
#' @importFrom tidyr replace_na
#' @importFrom rlang !! sym
#'
#' @noRd
sm_val_remover <- function(q_name, value, filter_expr, var_uuid, var_index, relevant, sheet, env) {
  choices <- get_choices(q_name, env)
  cols <- get_binary_cols(q_name, sheet, choices, env)
  env$object[[sheet]] <- mutate_at(env$object[[sheet]], c(q_name, cols),
                                   ~ ifelse(!!filter_expr & !!sym(q_name) == "", NA, .x))
}

#' Get binary columns for select_multiple data
#'
#' @importFrom glue glue glue_collapse
#' @importFrom dplyr select matches
#'
#' @noRd
get_binary_cols <- function(q_name, sheet, choices, env) {
  search_rgx <- glue("(\\b{q_name})(\\.|\\/)({choices}\\b)")
  search_rgx <- glue_collapse(search_rgx, sep = "|")
  unique(names(env$object[[sheet]] %>%
                 select(matches(search_rgx))))
}

#' Add selected option for select_multiple response
#'
#' @importFrom dplyr filter select matches mutate
#' @importFrom stringr str_detect
#' @importFrom glue glue
#' @importFrom rlang abort !! sym :=
#'
#' @noRd
add_option <- function(q_name, value, var_uuid, var_index, relevant, env) {
  if (!detect_select_multiple(q_name, env)) {
    abort(
      glue(
        "add_option failed to add {value} to {q_name} since it is not a select_multiple question"
      )
    )
  } else {
    filter_expr <- filter_expr_generator(var_index, var_uuid, relevant)
    sheet <- get_sheet(q_name, env)
    binary_name <- get_binary_cols(q_name, sheet, value, env)
    choices <- get_choices(q_name, env)

    if (is.null(filter_expr)) {
      filter_expr <- rep(T, nrow(env$object[[sheet]]))
      warn(
        glue(
          "Adding {value} to all instances of {q_name} since no UUID, index, position or relevant logic provided"
        )
      )
    }

    sm_na_replacer(q_name, value, filter_expr, var_uuid, var_index, relevant, sheet, choices, env)
    env$object[[sheet]] <- mutate(env$object[[sheet]],
                                  !!q_name := ifelse(!!filter_expr,
                                                     sm_str_adder(!!sym(q_name), value, choices),
                                                     !!sym(q_name)),
                                  !!binary_name := ifelse(!!filter_expr,
                                                          TRUE,
                                                          !!sym(binary_name)))
  }
}

#' Remove selected option from select_multiple questions
#'
#' @importFrom dplyr filter select matches mutate
#' @importFrom stringr str_detect
#' @importFrom rlang warn !! sym :=
#' @importFrom glue glue
#'
#' @noRd
remove_option <- function(q_name, value, var_uuid, var_index, relevant, env) {
  sheet <- get_sheet(q_name, env)

  if (!detect_select_multiple(q_name, env)) {
    change_response(q_name, NA, var_uuid, var_index, env)
    warn(
      glue(
        "remove_option is removing the entire response {value} since {q_name} is a select_one question"
      )
    )
  } else {
    filter_expr <- filter_expr_generator(var_index, var_uuid, relevant)
    binary_name <- get_binary_cols(q_name, sheet, value, env)
    env$object[[sheet]] <- mutate(env$object[[sheet]],
                                  !!q_name := ifelse(!!filter_expr,
                                                     sm_str_remover(!!sym(q_name), value),
                                                     !!sym(q_name)),
                                  !!binary_name := ifelse(!!filter_expr, FALSE, !!sym(binary_name)))
    sm_val_remover(q_name, value, filter_expr, var_uuid, var_index, relevant, sheet, env)
  }
}

