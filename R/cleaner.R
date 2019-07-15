#' Clean XLSForm survey data
#'
#' @importFrom rlang current_env abort
#' @importFrom purrr pmap
#'
kobold_cleaner <- function(object) {

  # Getting current environment
  env <- current_env()

  # General cleaning function ------------------------------------------------------------------------------

  general_cleaner <- function(type, name, value, sheet, uuid, index, relevant, cleaning_row, env) {
    print(cleaning_row)
    if (type == "change_response") {
      change_response(
        name,
        value,
        uuid,
        index,
        relevant,
        env
      )
    } else if (type == "remove_survey") {
      remove_entry(
        "data",
        uuid,
        index,
        relevant,
        env
      )
    } else if (type == "remove_loop_entry") {
      remove_entry(
        sheet,
        uuid,
        index,
        relevant,
        env
      )
    } else if (type == "remove_option") {
      remove_option(
        name,
        value,
        uuid,
        index,
        relevant,
        env
      )
    } else if (type == "add_option") {
      add_option(
        name,
        value,
        uuid,
        index,
        relevant,
        env
      )
    } else {
      abort(glue("Cleaning type {type} is incorrect"))
    }
  }

  pmap(
    list(
      object$cleaning$type,
      object$cleaning$name,
      object$cleaning$value,
      object$cleaning$sheet,
      object$cleaning$uuid,
      object$cleaning$index,
      object$cleaning$relevant,
      1:nrow(object$cleaning)
    ),
    general_cleaner, env
  )

  # Updating the relevant logic for the data
  object <- relevant_updater(object)

  # Converting the columns for each type
  class_converter(env)

  return(object)
}

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
                                         funs(ifelse(uuid == chg_uuid, NA, .)))
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
                                         funs(ifelse(uuid == chg_uuid, NA, .)))
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

#' Add selected option for select_multiple response
#'
#' @importFrom dplyr filter select matches mutate
#' @importFrom stringr str_detect
#' @importFrom glue glue
#' @importFrom rlang abort !! sym :=
#'
#' @noRd
add_option <- function(q_name, value, add_uuid, add_index, relevant, env) {
  sheet <- filter(env$object$survey, name == q_name)$sheet

  if (!str_detect(c(filter(env$object$survey, name == q_name)$type), "^.*(select_multiple|select multiple)")) {
    abort(
      glue(
        "add_option failed to add {value} to {q_name} since it is not a select_multiple question"
      )
    )
  } else {
    binary_name <- unique(names(env$object[[sheet]] %>%
                                  select(matches(
                                    paste0("(\\b", q_name, ")(\\.|\\/)(", value, "\\b)")
                                  ))))
    l_name <- filter(env$object$survey, name == q_name)$list_name
    choices <- filter(env$object$choices, list_name == l_name)$name
  }
  if (!is.na(add_index)) {
    env$object[[sheet]] <- mutate(env$object[[sheet]],
                                  !!q_name := ifelse(index == add_index,
                                                     select_mul_str_adder(!!sym(q_name), value, choices),
                                                     !!sym(q_name)),
                                  !!binary_name := ifelse(index == add_index,
                                                          TRUE,
                                                          !!sym(binary_name)))
  } else if (!is.na(add_uuid)) {
    env$object[[sheet]] <- mutate(env$object[[sheet]],
                               !!q_name := ifelse(uuid == add_uuid,
                                                  select_mul_str_adder(!!sym(q_name), value, choices),
                                                  !!sym(q_name)),
                               !!binary_name := ifelse(uuid == add_uuid,
                                                       TRUE,
                                                       !!sym(binary_name)))
  } else if (!is.na(relevant)) {
    env$object[[sheet]] <- mutate(env$object[[sheet]],
                               !!q_name := ifelse(!!convert_relevant(relevant),
                                                  select_mul_str_adder(!!sym(q_name), value, choices),
                                                  !!sym(q_name)),
                               !!binary_name := ifelse(!!convert_relevant(relevant),
                                                       TRUE,
                                                       !!sym(binary_name)))
  } else {
    warn(
      glue(
        "Adding {value} to all instances of {q_name} since no UUID or relevant logic provided"
      )
    )

    env$object[[sheet]] <- mutate(
      env$object[[sheet]],
      !!q_name := select_mul_str_adder(!!sym(q_name), value, choices),
      !!binary_name := TRUE
    )
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
remove_option <- function(q_name, value, rem_uuid, rem_index, relevant, env) {
  sheet <- filter(env$object$survey, name == q_name)$sheet

  if (!str_detect(c(filter(env$object$survey, name == q_name)$type), "^.*(select_multiple|select multiple)")) {
    change_response(q_name, NA, rem_uuid, rem_index, relevant, env)
    warn(
      glue(
        "remove_option is removing the entire response {value} since {q_name} is a select_one question"
      )
    )
  } else {
    # Here we get the name of the select_multiple binary column to change the value for
    binary_name <- unique(names(env$object[[sheet]] %>%
                                  select(matches(paste0("(\\b", q_name, ")(\\.|\\/)(", value, "\\b)")))))

    if (!is.na(rem_index)) {
      ## making the changes if based on UUID
      env$object[[sheet]] <- mutate(env$object[[sheet]], !!q_name := ifelse(index == rem_index,
                                                                            select_mul_str_removal(!!sym(q_name), value),
                                                                            !!sym(q_name)),
                                    !!binary_name := ifelse(index == rem_index, FALSE, !!sym(binary_name)))
    } else if (!is.na(rem_uuid)) {
      ## making the changes if based on UUID
      env$object[[sheet]] <- mutate(env$object[[sheet]], !!q_name := ifelse(uuid == rem_uuid,
                                                                            select_mul_str_removal(!!sym(q_name), value),
                                                                            !!sym(q_name)),
                                    !!binary_name := ifelse(uuid == rem_uuid, FALSE, !!sym(binary_name)))
    } else if (!is.na(relevant)) {
      # making the changes if based on relevant logic
      env$object[[sheet]] <- mutate(env$object[[sheet]],
                                    !!q_name := ifelse(!!convert_relevant(relevant),
                                                       select_mul_str_removal(!!sym(q_name), value),
                                                       !!sym(q_name)),
                                    !!binary_name := ifelse(!!convert_relevant(relevant), FALSE, !!sym(binary_name)))
    }
  }
}

#' Remove entries from loop data sheets
#'
#' @importFrom dplyr filter
#' @importFrom rlang !!
#'
#' @noRd
remove_entry <- function(sheet, rem_uuid, rem_index, relevant, env) {
  if (!is.na(rem_index)) {
    env$object[[sheet]] <- filter(env$object[[sheet]], !(index == rem_index))
  } else if (!is.na(rem_uuid)) {
    env$object[[sheet]] <- filter(env$object[[sheet]], !(uuid == rem_uuid))
  } else {
    stopifnot(!is.na(relevant))
    relevant <- convert_relevant(relevant)
    if (!is.na(match("uuid", names(env$object[[sheet]])))) {
      rem_uuid <- filter(env$object[[sheet]], !(!!relevant))$uuid
    }
    else if (!is.na(match("index", names(env$object[[sheet]])))) {
      rem_index <- filter(env$object[[sheet]], !(!!relevant))$index
    }
    env$object[[sheet]] <- filter(env$object[[sheet]], !(!!relevant))
  }
  child_entry_remover(sheet, rem_uuid, rem_index, env)
}

#' Remove entries from nested loop sheets
#'
#' @importFrom dplyr filter
#' @importFrom purrr pmap
#'
#' @noRd
child_entry_remover <- function(sheet, rem_uuid, rem_index, env) {
  children <- filter(env$object$data_sheets, parent == sheet)$sheets
  sheet_num <- length(children)
  if (!(is.na(rem_index)) & sheet_num > 0) {
    for (i in 1:sheet_num) {
      indices <- filter(env$object[[children[i]]], parent_index %in% rem_index)$index
      newborns <- filter(env$object$data_sheets, parent == children[i])$sheets
      env$object[[children[i]]] <- filter(env$object[[children[i]]], !(parent_index %in% rem_index))
      if (length(newborns) > 0) {
        vars <- list(newborns,
                     rem_uuid,
                     indices)
        pmap(vars, child_entry_remover, env)
      }
    }
  } else if (!(is.na(rem_uuid)) & sheet_num > 0) {
    for (i in 1:sheet_num) {
      env$object[[children[i]]] <- filter(env$object[[children[i]]], !(uuid %in% rem_uuid))
      newborns <- filter(env$object$data_sheets, parent == sheet)$sheets
      if (length(newborns) > 0) {
        vars <- list(newborns,
                     rem_uuid,
                     rem_index)
        pmap(vars, child_entry_remover, env)
      }
    }
  }
}


#' Generate filter expression for cleaning functions
#'
#' @importFrom glue glue
#' @importFrom rlang parse_expr
#'
filter_expr_generator <- function(index, uuid, relevant) {
  if (!is.na(index)) {
    parse_expr(glue("index == {index}"))
  } else if (!is.na(uuid)) {
    parse_expr(glue("uuid == {uuid}"))
  } else if (!is.na(relevant)) {
    convert_relevant(relevant)
  }
}
