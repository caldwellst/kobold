#' Change response for question
#'
#' @importFrom dplyr filter mutate select matches funs
#' @importFrom lubridate as_date
#' @importFrom stringr str_detect str_split
#' @importFrom glue glue glue_collapse
#' @importFrom rlang sym
#' @importFrom purrr map
#'
#' @noRd
change_response <- function(q_name, value, chg_uuid, chg_index, relevant, env) {
  sheet <- filter(env$object$survey, name == q_name)$sheet
  type <- filter(env$object$survey, name == q_name)$type

  if (type %in% c("decimal", "integer", "range")) {
    value <- as.integer(value)
  } else if (type %in% c("date", "today")) {
    value <- as_date(as.integer(value), origin = "1899-12-30")
  }

  select_multiple <- str_detect(c(filter(env$object$survey, name == q_name)$type), "^.*(select_multiple|select multiple)")

  if (!is.na(chg_index)) {
    if (select_multiple) {
      if (is.na(value)) {
        l_name <- filter(env$object$survey, name == q_name)$list_name
        choices <- filter(env$object$choices, list_name == l_name)$name
        search_rgx <- glue("(\\b{q_name})(\\.|\\/)({choices}\\b)")
        search_rgx <- glue_collapse(search_rgx, sep = "|")
        all_binary_cols <- unique(names(env$object[[sheet]] %>%
                                          select(matches(search_rgx))))
        all_cols <- c(q_name, all_binary_cols)
        env$object[[sheet]] <- mutate_at(env$object[[sheet]], all_cols,
                                         list(~ ifelse(uuid == chg_uuid, NA, .)))
      } else {
        new_values <- unlist(str_split(value, " "))
        original_values <- filter(env$object[[sheet]], index == chg_index)[[q_name]]
        original_values <- str_split(original_values, " ")
        add_values <- map(original_values, function(x) new_values[!(new_values %in% x)])
        remove_values <- map(original_values, function(x) x[!(x %in% new_values)])
        map(add_values, function(z) map(z, ~ add_option(q_name, .x, chg_uuid, chg_index, relevant, env)))
        map(remove_values, function(z) map(z, ~ remove_option(q_name, .x, chg_uuid, chg_index, relevant, env)))
      }
    } else {
      env$object[[sheet]] <- mutate(env$object[[sheet]],
                                    !!q_name := ifelse(index == chg_index, value, !!sym(q_name)))
    }
  } else if (!is.na(chg_uuid)) {
    if (select_multiple) {
      if (is.na(value)) {
        l_name <- filter(env$object$survey, name == q_name)$list_name
        choices <- filter(env$object$choices, list_name == l_name)$name
        search_rgx <- glue("(\\b{q_name})(\\.|\\/)({choices}\\b)")
        search_rgx <- glue_collapse(search_rgx, sep = "|")
        all_binary_cols <- unique(names(env$object[[sheet]] %>%
                                          select(matches(search_rgx))))
        all_cols <- c(q_name, all_binary_cols)
        env$object[[sheet]] <- mutate_at(env$object[[sheet]], all_cols,
                                         list(~ ifelse(uuid == chg_uuid, NA, .)))
      } else {
        new_values <- unlist(str_split(value, " "))
        original_values <- filter(env$object[[sheet]], uuid == chg_uuid)[[q_name]]
        original_values <- str_split(original_values, " ")
        add_values <- map(original_values, function(x) new_values[!(new_values %in% x)])
        remove_values <- map(original_values, function(x) x[!(x %in% new_values)])
        if(!is.na(add_values)) {
          map(add_values, function(z) map(z, ~ add_option(q_name, .x, chg_uuid, chg_index, relevant, env)))
        }
        if(!is.na(remove_values)) {
          map(remove_values, function(z) map(z, ~ remove_option(q_name, .x, chg_uuid, chg_index, relevant, env)))
        }
      }
    } else {
      env$object[[sheet]] <- mutate(env$object[[sheet]],
                                    !!q_name := ifelse(uuid == chg_uuid, value, !!sym(q_name)))
    }
  } else if (!is.na(relevant)) {
    relevant <- convert_relevant(relevant)
    env$object[[sheet]] <- mutate(env$object[[sheet]],
                                  !!q_name := ifelse(ifelse(is.na(!!relevant), FALSE, !!relevant), value, !!sym(q_name)))

    if (select_multiple) {
      l_name <- filter(env$object$survey, name == q_name)$list_name
      choices <- filter(env$object$choices, list_name == l_name)$name
      search_rgx <- glue("(\\b{q_name})(\\.|\\/)({choices}\\b)")
      search_rgx <- glue_collapse(search_rgx, sep = "|")
      all_binary_cols <- unique(names(env$object[[sheet]] %>%
                                        select(matches(search_rgx))))
      all_col_change <- ifelse(is.na(value), NA, 0)

      value_split <- as.character(unlist(str_split(value, pattern = " ")))
      search_rgx <- glue("(\\b{q_name})(\\.|\\/)({value_split}\\b)")
      search_rgx <- glue_collapse(search_rgx, sep = "|")
      selected_binary_cols <- unique(names(env$object[[sheet]] %>%
                                             select(matches(search_rgx))))

      for (i in 1:length(all_binary_cols)) {
        env$object[[sheet]] <- mutate(env$object[[sheet]],
                                      !!all_binary_cols[i] := ifelse(!!relevant, all_col_change, !!sym(all_binary_cols[i])))
      }

      if (!is.na(value)) {
        for (i in 1:length(selected_binary_cols)) {
          env$object[[sheet]] <- mutate(env$object[[sheet]],
                                        !!selected_binary_cols[i] := ifelse(!!relevant, 1, !!sym(selected_binary_cols[i])))
        }
      }
    }
  }

  else {
    warn(
      glue(
        "Changing all values in {q_name} to {value} since no UUID or relevant logic provided"
      )
    )
    env$object[[sheet]] <- mutate(env$object[[sheet]], !!q_name := value)
  }
}
