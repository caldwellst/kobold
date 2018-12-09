#' @importFrom dplyr mutate filter
#' @importFrom rlang sym
#' @importFrom glue glue
#' @importFrom purrr pmap map map_chr map_lgl
#' @importFrom stringr str_remove str_trim str_replace str_c
kobold_cleaner <- function(kobold_file) {
  # Function to remove surveys based on either a UUID or relevant logic

  remove_survey <- function(uuid, relevant) {
    if (uuid != "") {
      kobold_file$data <<- filter(kobold_file$data,!(X_uuid == uuid))
    }
    else {
      stopifnot(!is.na(relevant))
      kobold_file$data <<-
        filter(kobold_file$data,!(!!convert_xls_code(relevant)))
    }
  }

  # Change response function ------------------------------------------------------------------------------
  # Function to change value in a columns row(s) based on name of the cell, new value to place, and UUID or relevant logic.

  change_response <- function(name, value, uuid, relevant) {
    if (!is.na(uuid)) {
      kobold_file$data <<-
        mutate(kobold_file$data,
               !!name := ifelse(X_uuid == uuid, value,!!sym(name)))
    }

    else if (!is.na(relevant)) {
      kobold_file$data <<-
        mutate(kobold_file$data,
               !!name := ifelse(!!convert_xls_code(relevant), value,!!sym(name)))
    }

    else {
      warning(
        glue(
          "Changing all values in {name} to {value} since no UUID or relevant logic provided"
        )
      )
      kobold_file$data <<-
        mutate(kobold_file$data,!!name := value)
    }
  }

  # Remove option function ------------------------------------------------------------------------------
  # Removes singular value from a response. For non-select_multiple questions, just instead run
  # change_response, since there is no need to deal with multiple response options.

  remove_option <- function(q_name, value, uuid, relevant) {
    if (!str_detect(c(filter(kobold_file$survey, name == q_name)$type), "^.*(select_multiple|select multiple)")) {
      change_response(q_name, value, uuid, relevant)
      warning(
        glue(
          "remove_option is removing the entire response {value} since {q_name} is a select_one question"
        )
      )
    }

    else {
      ## Here we get the name of the select_multiple binary column to change the value for
      binary_name <- unique(names(kobold_file$data %>%
                                    select(matches(
                                      paste0("(\\b", q_name, ")(.)(", value, "\\b)")
                                    ),-one_of(
                                      c(kobold_file$survey$name)
                                    ))))
    }

    if (!is.na(uuid)) {
      ## making the changes if based on UUID
      kobold_file$data <<- mutate(
        kobold_file$data,!!q_name := ifelse(
          X_uuid == uuid,
          select_mul_str_removal(!!sym(q_name), value),!!sym(q_name)
        ),!!binary_name := ifelse(X_uuid == uuid,
                                  FALSE,!!sym(binary_name))
      )
    }

    else if (!is.na(relevant)) {
      # making the changes if based on relevant logic
      kobold_file$data <<- mutate(
        kobold_file$data,!!q_name := ifelse(
          !!convert_xls_code(relevant),
          select_mul_str_removal(!!sym(q_name), value),!!sym(q_name)
        ),!!binary_name := ifelse(
          !!convert_xls_code(relevant),
          FALSE,!!sym(binary_name)
        )
      )
    }
  }

  # Add option function ------------------------------------------------------------------------------

  add_option <- function(q_name, value, uuid, relevant) {
    if (!str_detect(c(filter(kobold_file$survey, name == q_name)$type), "^.*(select_multiple|select multiple)")) {
      stop(
        glue(
          "add_option failed to add {value} to {q_name} since it is not a select_multiple question"
        )
      )
    }
    # Generating the name of the binary column and choices list
    else {
      binary_name <- unique(names(kobold_file$data %>%
                                    select(matches(
                                      paste0("(\\b", q_name, ")(.)(", value, "\\b)")
                                    ))))
      l_name <-
        filter(kobold_file$survey, name == q_name)$list_name
      choices <-
        filter(kobold_file$choices, list_name == l_name)$name
    }

    if (!is.na(uuid)) {
      kobold_file$data <<- mutate(
        kobold_file$data,!!q_name := ifelse(
          X_uuid == uuid,
          select_mul_str_adder(!!sym(q_name), value, choices),!!sym(q_name)
        ),!!binary_name := ifelse(X_uuid == uuid,
                                  TRUE,!!sym(binary_name))
      )
    }

    else if (!is.na(relevant)) {
      kobold_file$data <<- mutate(
        kobold_file$data,!!q_name := ifelse(
          !!convert_xls_code(relevant),
          select_mul_str_adder(!!sym(q_name), value, choices),!!sym(q_name)
        ),!!binary_name := ifelse(!!convert_xls_code(relevant),
                                  TRUE,!!sym(binary_name))
      )
    }
  }

  # General cleaning function ------------------------------------------------------------------------------

  general_cleaner <- function(type, name, value, uuid, relevant) {
    if (type == "change_response") {
      change_response(name, value, uuid, relevant)
    } else if (type == "remove_survey") {
      remove_survey(uuid, relevant)
    } else if (type == "remove_option") {
      remove_option(
        q_name = name,
        value = value,
        uuid = uuid,
        relevant = relevant
      )
    } else if (type == "add_option") {
      add_option(
        q_name = name,
        value = value,
        uuid = uuid,
        relevant = relevant
      )
    } else {
      stop(glue("Cleaning type {type} is incorrect"))
    }
  }

  pmap(
    list(
      kobold_file$cleaning$type,
      kobold_file$cleaning$name,
      kobold_file$cleaning$value,
      kobold_file$cleaning$uuid,
      kobold_file$cleaning$relevant
    ),
    general_cleaner
  )

  return(kobold_file)
}

#' Remove select_multiple value
#'
#' @importFrom glue glue
#' @importFrom stringr str_trim str_replace str_remove
select_mul_str_removal <- function(column, value) {
  option = glue("\\b{value}\\b")
  str_trim(str_replace(str_remove(column, option), "  ", " "))
}

#' Add select_multiple value
#'
#' @importFrom glue glue
#' @importFrom stringr str_split
#' @importFrom purrr map
select_mul_str_adder <- function(column, value, choices) {
  split <- str_split(column, pattern = " ")
  exist_test <- map_lgl(split, function(x)
    is.na(match(value, x)))
  split <- ifelse(exist_test, map(split, append, value), split)
  split <- map(split, function(x)
    x[order(match(x, choices))])
  map_chr(split, str_c, collapse = " ")
}
