#' @importFrom dplyr mutate filter
#' @importFrom rlang sym abort warn
#' @importFrom glue glue
#' @importFrom purrr pmap map map_chr map_lgl
#' @importFrom stringr str_remove str_trim str_replace str_c str_detect
kobold_cleaner <- function(object) {

  # Function to remove surveys based on either a UUID or relevant logic

  remove_survey <- function(sheet, rem_uuid, relevant) {
    if (!is.na(rem_uuid)) {
      object[[sheet]] <<- filter(object[[sheet]], !(uuid == rem_uuid))
    }
    else {
      stopifnot(!is.na(relevant))
      object[[sheet]] <<-
        filter(object[[sheet]], !(!!convert_xls_code(relevant)))
    }
  }

  # Change response function ------------------------------------------------------------------------------
  # Function to change value in a columns row(s) based on name of the cell, new value to place, and UUID or relevant logic.

  change_response <- function(q_name, value, chg_uuid, relevant) {
    sheet <- filter(object$survey, name == q_name)$group

    if (!is.na(chg_uuid)) {
      object[[sheet]] <<-
        mutate(object[[sheet]],
               !!q_name := ifelse(uuid == chg_uuid, value, !!sym(q_name)))
    }

    else if (!is.na(relevant)) {
      object[[sheet]] <<-
        mutate(object[[sheet]],
               !!q_name := ifelse(!!convert_xls_code(relevant), value, !!sym(q_name)))
    }

    else {
      warn(
        glue(
          "Changing all values in {q_name} to {value} since no UUID or relevant logic provided"
        )
      )
      object[[sheet]] <<-
        mutate(object[[sheet]], !!q_name := value)
    }
  }

  # Remove option function ------------------------------------------------------------------------------
  # Removes singular value from a response. For non-select_multiple questions, just instead run
  # change_response, since there is no need to deal with multiple response options.

  remove_option <- function(q_name, value, rem_uuid, relevant) {
    sheet <- filter(object$survey, name == q_name)$group

    if (!str_detect(c(filter(object$survey, name == q_name)$type), "^.*(select_multiple|select multiple)")) {
      change_response(q_name, NA, rem_uuid, relevant)
      warn(
        glue(
          "remove_option is removing the entire response {value} since {q_name} is a select_one question"
        )
      )
    }

    else {
      # Here we get the name of the select_multiple binary column to change the value for
      binary_name <- unique(names(object[[sheet]] %>%
                                    select(matches(paste0("(\\b", q_name, ")(.)(", value, "\\b)")),
                                           -one_of(c(object$survey$name)))))
    }

    if (!is.na(rem_uuid)) {
      ## making the changes if based on UUID
      object[[sheet]] <<- mutate(
        object[[sheet]], !!q_name := ifelse(
          uuid == rem_uuid,
          select_mul_str_removal(!!sym(q_name), value),!!sym(q_name)
        ), !!binary_name := ifelse(uuid == rem_uuid,
                                  FALSE,!!sym(binary_name))
      )
    }

    else if (!is.na(relevant)) {
      # making the changes if based on relevant logic
      object[[sheet]] <<- mutate(
        object[[sheet]], !!q_name := ifelse(
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

  add_option <- function(q_name, value, add_uuid, relevant) {
    sheet <- filter(object$survey, name == q_name)$group

    if (!str_detect(c(filter(object$survey, name == q_name)$type), "^.*(select_multiple|select multiple)")) {
      abort(
        glue(
          "add_option failed to add {value} to {q_name} since it is not a select_multiple question"
        )
      )
    }

    # Generating the name of the binary column and choices list
    else {
      binary_name <- unique(names(object[[sheet]] %>%
                                    select(matches(
                                      paste0("(\\b", q_name, ")(.)(", value, "\\b)")
                                    ))))
      l_name <-
        filter(object$survey, name == q_name)$list_name
      choices <-
        filter(object$choices, list_name == l_name)$name
    }

    if (!is.na(add_uuid)) {
      object[[sheet]] <<- mutate(
        object[[sheet]],
        !!q_name := ifelse(uuid == add_uuid,
                           select_mul_str_adder(!!sym(q_name),
                                                value,
                                                choices),
                           !!sym(q_name)),
        !!binary_name := ifelse(uuid == add_uuid,
                                TRUE,
                                !!sym(binary_name)))
    }

    else if (!is.na(relevant)) {
      object[[sheet]] <<- mutate(
        object[[sheet]],
        !!q_name := ifelse(!!convert_xls_code(relevant),
                           select_mul_str_adder(!!sym(q_name), value, choices),
                           !!sym(q_name)),
        !!binary_name := ifelse(!!convert_xls_code(relevant),
                                TRUE,
                                !!sym(binary_name)))
    }

    else {
      warn(
        glue(
          "Adding {value} to all instances of {q_name} since no UUID or relevant logic provided"
        )
      )

      object[[sheet]] <<- mutate(
        object[[sheet]],
        !!q_name := select_mul_str_adder(!!sym(q_name), value, choices),
        !!binary_name := TRUE
      )
    }
  }

  # Relevant logic updater

  relevant_updater <- function(q_name, chg_uuid, relevant) {
    srch_term <- glue("\\$\\{(q_name)\\}",
                      .open = "(",
                      .close = ")")
    indices <- str_which(object$survey$relevant, srch_term)
    indices_num <- length(indices)

    if (indices_num > 0) {
      vars <- object$survey$name[indices]
      relevants <- object$survey$relevant[indices]

      for (i in 1:indices_num) {
        sheet <- filter(object$survey, name == vars[i])$group
        if(!is.na(chg_uuid)) {
          object[[sheet]] <<-
            mutate(
              object[[sheet]],
              !!vars[i] := ifelse(uuid == chg_uuid & !(!!convert_xls_code(relevants[i])),
                                  NA,
                                  !!sym(vars[i])))
        }

        else if(!is.na(relevant)) {
          object[[sheet]] <<-
            mutate(
              object[[sheet]],
              !!vars[i] := ifelse(!!convert_xls_code(relevant) & !(!!convert_xls_code(relevants[i])),
                                  NA,
                                  !!sym(vars[i])))
        }

        relevant_updater(vars[i], chg_uuid, relevant)
      }
    }
  }

  # General cleaning function ------------------------------------------------------------------------------

  general_cleaner <- function(type, name, value, uuid, relevant) {
    if (type == "change_response") {
      change_response(name, value, uuid, relevant)
    } else if (type == "remove_survey") {
      map(object$data_sheets,
          remove_survey,
          uuid,
          relevant)
    } else if (type == "remove_option") {
      remove_option(
        name,
        value,
        uuid,
        relevant
      )
    } else if (type == "add_option") {
      add_option(
        name,
        value,
        uuid,
        relevant
      )
    } else {
      abort(glue("Cleaning type {type} is incorrect"))
    }
    relevant_updater(name, uuid, relevant)


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

#' Add select_multiple valeuw
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
