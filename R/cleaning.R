#' @importFrom dplyr mutate filter
#' @importFrom rlang sym
#' @importFrom glue glue
#' @importFrom purrr pmap map map_chr map_lgl
#' @importFrom stringr str_remove str_trim str_replace str_c str_detect
kobold_cleaner <- function(object) {

  # Identifying loop locations for questions
  object$survey$group <- NA
  begin_reg <- "^.*(begin_repeat|begin repeat)"
  end_reg <- "^.*(end_repeat|end repeat)"
  group <- "data"
  i <- 1

  while (i <= nrow(object$survey)) {
    type <- object$survey$type[i]

    if (str_detect(type, begin_reg)) {
      group <- object$survey$name[i]
    }

    object$survey$group[i] <- group

    if (str_detect(type, end_reg)) {
      group <- "data"
    }

    i <- i + 1
  }

  # Function to remove surveys based on either a UUID or relevant logic

  remove_survey <- function(uuid, relevant) {
    if (uuid != "") {
      object$data <<- filter(object$data,!(X_uuid == uuid))
    }
    else {
      stopifnot(!is.na(relevant))
      object$data <<-
        filter(object$data, !(!!convert_xls_code(relevant)))
    }
  }

  # Change response function ------------------------------------------------------------------------------
  # Function to change value in a columns row(s) based on name of the cell, new value to place, and UUID or relevant logic.

  change_response <- function(name, value, uuid, relevant) {
    if (!is.na(uuid)) {
      object$data <<-
        mutate(object$data,
               !!name := ifelse(X_uuid == uuid, value,!!sym(name)))
    }

    else if (!is.na(relevant)) {
      object$data <<-
        mutate(object$data,
               !!name := ifelse(!!convert_xls_code(relevant), value,!!sym(name)))
    }

    else {
      warning(
        glue(
          "Changing all values in {name} to {value} since no UUID or relevant logic provided"
        )
      )
      object$data <<-
        mutate(object$data,!!name := value)
    }
  }

  # Remove option function ------------------------------------------------------------------------------
  # Removes singular value from a response. For non-select_multiple questions, just instead run
  # change_response, since there is no need to deal with multiple response options.

  remove_option <- function(q_name, value, uuid, relevant) {
    if (!str_detect(c(filter(object$survey, name == q_name)$type), "^.*(select_multiple|select multiple)")) {
      change_response(q_name, value, uuid, relevant)
      warning(
        glue(
          "remove_option is removing the entire response {value} since {q_name} is a select_one question"
        )
      )
    }

    else {
      ## Here we get the name of the select_multiple binary column to change the value for
      binary_name <- unique(names(object$data %>%
                                    select(matches(
                                      paste0("(\\b", q_name, ")(.)(", value, "\\b)")
                                    ),-one_of(
                                      c(object$survey$name)
                                    ))))
    }

    if (!is.na(uuid)) {
      ## making the changes if based on UUID
      object$data <<- mutate(
        object$data,!!q_name := ifelse(
          X_uuid == uuid,
          select_mul_str_removal(!!sym(q_name), value),!!sym(q_name)
        ),!!binary_name := ifelse(X_uuid == uuid,
                                  FALSE,!!sym(binary_name))
      )
    }

    else if (!is.na(relevant)) {
      # making the changes if based on relevant logic
      object$data <<- mutate(
        object$data,!!q_name := ifelse(
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
    if (!str_detect(c(filter(object$survey, name == q_name)$type), "^.*(select_multiple|select multiple)")) {
      stop(
        glue(
          "add_option failed to add {value} to {q_name} since it is not a select_multiple question"
        )
      )
    }
    # Generating the name of the binary column and choices list
    else {
      binary_name <- unique(names(object$data %>%
                                    select(matches(
                                      paste0("(\\b", q_name, ")(.)(", value, "\\b)")
                                    ))))
      l_name <-
        filter(object$survey, name == q_name)$list_name
      choices <-
        filter(object$choices, list_name == l_name)$name
    }

    if (!is.na(uuid)) {
      object$data <<- mutate(
        object$data,!!q_name := ifelse(
          X_uuid == uuid,
          select_mul_str_adder(!!sym(q_name), value, choices),!!sym(q_name)
        ),!!binary_name := ifelse(X_uuid == uuid,
                                  TRUE,!!sym(binary_name))
      )
    }

    else if (!is.na(relevant)) {
      object$data <<- mutate(
        object$data,!!q_name := ifelse(
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
      object$cleaning$type,
      object$cleaning$name,
      object$cleaning$value,
      object$cleaning$uuid,
      object$cleaning$relevant
    ),
    general_cleaner
  )

  return(object)
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
