#' Function for converting non-select multiple columns to the proper type
#' @importFrom rlang caller_env env_names
#' @importFrom stringr str_c
#' @importFrom dplyr mutate_at one_of vars
convert_columns <- function(sheet, types, converter, env) {
  types <- str_c(types, collapse = "|")
  types <- str_c("^(?!.*select).*(", types, ").*")

  name_rows <- filter(env$object$survey,
                      str_detect(type, types))
  cnv_names <- name_rows$name
  sht_names <- names(env$object[[sheet]])
  cols <- unique(cnv_names[cnv_names %in% sht_names])
  suppressWarnings(suppressMessages(env$object[[sheet]] <-
                                      env$object[[sheet]] %>% mutate_at(vars(
                                        one_of(cols)
                                      ), converter)))
}

#' Function for converting columns of select multiple individual options to logical vectors
#' @importFrom dplyr filter select mutate_all matches
#' @importFrom stringr str_c str_detect
#' @importFrom glue glue
convert_select_multiple <- function(sheet, env) {
  sel_mul_reg <- "^.*(select_multiple|select multiple)"
  list_rows <- filter(env$object$survey, str_detect(type, sel_mul_reg))
  lists <- list_rows$list_name
  lists <- str_c(lists, collapse = "|")
  lists_reg <- glue("^.*({lists})s")

  choice_rows <- filter(env$object$choices, str_detect(list_name, lists_reg))
  choices <- choice_rows$name
  choices <- str_c(choices, collapse = "|")
  choices_reg <- str_c("(", choices, ")$")

  name_rows <- filter(env$object$survey, str_detect(type, sel_mul_reg))
  names <- name_rows$name
  names <- str_c(names, collapse = "|")
  names_reg <- str_c("^(", names, ")")

  survey_names <- env$object$survey$name

  suppressWarnings(suppressMessages(
    retype_cols <- env$object[[sheet]] %>%
      select(-one_of(survey_names)) %>%
      select(matches(names_reg)) %>%
      select(matches(choices_reg))
  ))

  retype_names <- unique(names(retype_cols))

  log_num <- function(x) {
    as.logical(as.numeric(x))
  }

  env$object[[sheet]][retype_names] <-
    mutate_all(env$object[[sheet]][retype_names], log_num)

}

#' Function for converting all columns in a data frame to the proper type
#' @importFrom purrr map
#' @importFrom lubridate as_datetime as_date
class_converter <- function(env) {
  map(env$object$data_sheets$sheets,
      convert_columns,
      c("decimal", "integer", "range"),
      as.numeric,
      env)

  map(env$object$data_sheets$sheets,
      convert_columns,
      c("start", "end", "time", "dateTime"),
      as_datetime,
      env)

  map(env$object$data_sheets$sheets,
      convert_columns,
      c("today", "date"),
      as_date,
      env)

  map(env$object$data_sheets$sheets,
      convert_select_multiple,
      env)
}
