#' @importFrom dplyr mutate filter
#' @importFrom rlang sym abort warn
#' @importFrom glue glue glue_collapse
#' @importFrom purrr pmap map map2 map_chr map_lgl
#' @importFrom stringr str_remove str_trim str_replace str_c str_detect str_which
kobold_cleaner <- function(object) {

  # Remove entries from nested repeat groups
  child_entry_remover <- function(sheet, rem_uuid, rem_index) {
    children <- filter(object$data_sheets, parent == sheet)$sheets
    sheet_num <- length(children)

    if (!(is.na(rem_uuid)) & sheet_num > 0) {
      for (i in 1:sheet_num) {
        object[[children[i]]] <<- filter(object[[children[i]]], !(uuid %in% rem_uuid))
        newborns <- filter(object$data_sheets, parent == sheet)$sheets
        if (length(newborns) > 0) {
          vars <- list(newborns,
                       rem_uuid,
                       rem_index)
          pmap(vars, child_entry_remover)
        }
      }
    }

    else if (!(is.na(rem_index)) & sheet_num > 0) {
      for (i in 1:sheet_num) {
        indices <- filter(object[[children[i]]], parent_index %in% rem_index)$index
        newborns <- filter(object$data_sheets, parent == sheet)$sheets
        object[[children[i]]] <<- filter(object[[children[i]]], !(parent_index %in% rem_index))
        if (length(newborns) > 0) {
          vars <- list(newborns,
                       rem_uuid,
                       indices)
          pmap(vars, child_entry_remover)
        }
      }
    }
  }

  # Remove entries from datasets
  remove_entry <- function(sheet, rem_uuid, rem_index, relevant) {
    if (!is.na(rem_uuid)) {
      object[[sheet]] <<- filter(object[[sheet]], !(uuid == rem_uuid))
    }

    else if (!is.na(rem_index)) {
      object[[sheet]] <<- filter(object[[sheet]], !(index == rem_index))
    }

    else {
      stopifnot(!is.na(relevant))
      relevant <- convert_xls_code(relevant)
      if (!is.na(match("uuid", names(object[[sheet]])))) {
        rem_uuid <- filter(object[[sheet]], !(!!relevant))$uuid
      }
      else if (!is.na(match("index", names(object[[sheet]])))) {
        rem_index <- filter(object[[sheet]], !(!!relevant))$index
      }
      object[[sheet]] <<- filter(object[[sheet]], !(!!relevant))
    }
    child_entry_remover(sheet, rem_uuid, rem_index)
  }

  # Change response function ------------------------------------------------------------------------------
  # Function to change value in a columns row(s) based on name of the cell, new value to place, and UUID or relevant logic.

  change_response <- function(q_name, value, chg_uuid, relevant) {
    sheet <- filter(object$survey, name == q_name)$sheet

    if (!is.na(chg_uuid)) {
      object[[sheet]] <<- mutate(object[[sheet]],
                                 !!q_name := ifelse(uuid == chg_uuid, value, !!sym(q_name)))
    }

    else if (!is.na(relevant)) {
      object[[sheet]] <<- mutate(object[[sheet]],
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
    sheet <- filter(object$survey, name == q_name)$sheet

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
    sheet <- filter(object$survey, name == q_name)$sheet

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
      l_name <- filter(object$survey, name == q_name)$list_name
      choices <- filter(object$choices, list_name == l_name)$name
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

  # Updating for relevant logic used on separate sheets
  separate_relevants <- function(original_sheet, sheet, q_name, relevant, chg_uuid, chg_index, chg_relevant) {
    select_multiple <- FALSE
    if (str_detect(c(filter(object$survey, name == q_name)$type), "^.*(select_multiple|select multiple)")) {
      select_multiple <- TRUE
      l_name <- filter(object$survey, name == q_name)$list_name
      choices <- filter(object$choices, list_name == l_name)$name
      search_rgx <- glue("(\\b{q_name})(.)({choices}\\b)")
      search_rgx <- glue_collapse(search_rgx, sep = "|")
      binary_names <- unique(names(object[[sheet]] %>%
                                     select(matches(search_rgx))))
    }

    # Get the UUID from the main sheet to connect to separate sheets
    if (!is.na(chg_uuid)) {
      chg_uuid <- filter(object[[original_sheet]], uuid %in% chg_uuid & !(!!convert_xls_code(relevant)))$uuid
      object[[sheet]] <<- mutate(object[[sheet]],
                                 !!q_name := ifelse(uuid == chg_uuid,
                                                    NA,
                                                    !!sym(q_name)))
      if (select_multiple) {
        for (i in 1:length(binary_names)) {
          object[[sheet]] <<- mutate(object[[sheet]],
                                     !!binary_names[i] := ifelse(uuid == chg_uuid,
                                                                 NA,
                                                                 !!sym(binary_names[i])))
        }
      }
    }

    # Get the index from the main sheet to connect to separate sheets
    else if (!is.na(chg_index)) {

      sheet_chain <- filter(object$data_sheets, sheets == sheet)$parent

      while (sheet_chain[1] != original_sheet) {
        parent <- filter(object$data_sheets, sheets == sheet_chain[1])$parent
        sheet_chain <- append(sheet_chain, parent, before = 0)
      }

      sheet_chain <- append(sheet_chain, sheet)

      chg_index <- filter(object[[original_sheet]], index %in% chg_index & !(!!convert_xls_code(relevant)))$index
      for (i in 2:length(sheet_chain)) {
        chg_index <- filter(object[[sheet_chain[i]]], parent_index %in% chg_index)$index
      }

      object[[sheet]] <<- mutate(object[[sheet]],
                                 !!q_name := ifelse(index == chg_index,
                                                    NA,
                                                    !!sym(q_name)))
      if (select_multiple) {
        for (i in 1:length(binary_names)) {
          object[[sheet]] <<- mutate(object[[sheet]],
                                     !!binary_names[i] := ifelse(index == chg_index,
                                                                 NA,
                                                                 !!sym(binary_names[i])))
        }
      }
    }
    else if (!is.na(relevant)) {
      if (!is.na(match("uuid", names(object[[sheet]])))) {
        chg_uuid <- filter(object[[original_sheet]], !(!!convert_xls_code(chg_relevant)) & !(!!convert_xls_code(relevant)))$uuid
        object[[sheet]] <<- mutate(object[[sheet]],
                                   !!q_name := ifelse(uuid == chg_uuid,
                                                      NA,
                                                      !!sym(q_name)))
        if (select_multiple) {
          for (i in 1:length(binary_names)) {
            object[[sheet]] <<- mutate(object[[sheet]],
                                       !!binary_names[i] := ifelse(uuid == chg_uuid,
                                                                   NA,
                                                                   !!sym(binary_names[i])))
          }
        }
      }

      else if (!is.na(match("index", names(object[[sheet]])))) {
        sheet_chain <- filter(object$data_sheets, sheets == sheet)$parent

        while (sheet_chain[1] != original_sheet) {
          parent <- filter(object$data_sheets, sheets == sheet_chain[1])$parent
          sheet_chain <- append(sheet_chain, parent, before = 0)
        }

        sheet_chain <- append(sheet_chain, sheet)
        chg_index <- filter(object[[original_sheet]], !(!!convert_xls_code(chg_relevant)) & !(!!convert_xls_code(relevant)))$index

        for (i in 2:length(sheet_chain)) {
          chg_index <- filter(object[[sheet_chain[i]]], parent_index %in% chg_index)$index
        }

        object[[sheet]] <<- mutate(object[[sheet]],
                                   !!q_name := ifelse(index == chg_index,
                                                      NA,
                                                      !!sym(q_name)))
        if (select_multiple) {
          for (i in 1:length(binary_names)) {
            object[[sheet]] <<- mutate(object[[sheet]],
                                       !!binary_names[i] := ifelse(index == chg_index,
                                                                   NA,
                                                                   !!sym(binary_names[i])))
          }
        }
      }
    }
    relevant_updater(q_name, chg_uuid, chg_index, chg_relevant)
  }

  # Updating for relevant logic used on the same sheet
  same_relevants <- function(sheet, q_name, relevant, chg_uuid, chg_index, chg_relevant) {
    select_multiple <- str_detect(c(filter(object$survey, name == q_name)$type), "^.*(select_multiple|select multiple)")
    if (select_multiple) {
      l_name <- filter(object$survey, name == q_name)$list_name
      choices <- filter(object$choices, list_name == l_name)$name
      search_rgx <- glue("(\\b{q_name})(.)({choices}\\b)")
      search_rgx <- glue_collapse(search_rgx, sep = "|")
      binary_names <- unique(names(object[[sheet]] %>%
                                    select(matches(search_rgx))))
    }

    if (!is.na(chg_uuid)) {
      object[[sheet]] <<- mutate(object[[sheet]],
                                 !!q_name := ifelse(uuid == chg_uuid & !(!!convert_xls_code(relevant)),
                                                     NA,
                                                     !!sym(q_name)))
      if (select_multiple) {
        for (i in 1:length(binary_names)) {
          object[[sheet]] <<- mutate(object[[sheet]],
                                     !!binary_names[i] := ifelse(uuid == chg_uuid & !(!!convert_xls_code(relevant)),
                                     NA,
                                     !!sym(binary_names[i])))
        }
      }
    } else if (!is.na(chg_index)) {
      object[[sheet]] <<- mutate(object[[sheet]],
                                 !!q_name := ifelse(index == chg_index & !(!!convert_xls_code(relevant)),
                                                    NA,
                                                    !!sym(q_name)))
      if (select_multiple) {
        for (i in 1:length(binary_names)) {
          object[[sheet]] <<- mutate(object[[sheet]],
                                     !!binary_names[i] := ifelse(index == chg_index & !(!!convert_xls_code(relevant)),
                                                                 NA,
                                                                 !!sym(binary_names[i])))
        }
      }
    } else if (!is.na(relevant)) {
      object[[sheet]] <<- mutate(object[[sheet]],
                                 !!q_name := ifelse(!!convert_xls_code(chg_relevant) & !(!!convert_xls_code(relevant)),
                                                    NA,
                                                    !!sym(q_name)))
      if (select_multiple) {
        for (i in 1:length(binary_names)) {
          object[[sheet]] <<- mutate(object[[sheet]],
                                     !!binary_names[i] := ifelse(!!convert_xls_code(chg_relevant) & !(!!convert_xls_code(relevant)),
                                                                 NA,
                                                                 !!sym(binary_names[i])))
        }
      }
    }
    relevant_updater(q_name, chg_uuid, chg_index, chg_relevant)
  }


  # Relevant logic updater

  relevant_updater <- function(q_name, chg_uuid, chg_index, chg_relevant) {
    srch_term <- glue("\\$\\{(q_name)\\}",
                      .open = "(",
                      .close = ")")
    indices <- str_which(object$survey$relevant, srch_term)
    indices_num <- length(indices)

    if (indices_num > 0) {
      vars <- object$survey$name[indices]
      relevants <- object$survey$relevant[indices]

      group_rgx <- "^.*(begin_group|begin group|begin repeat|begin_repeat)"
      group_indices <- str_detect(vars, group_rgx)
      groups <- vars[group_indices]
      groups_rel <- relevants[group_indices]

      vars <- vars[!group_indices]
      relevants <- relevants[!group_indices]

      if (length(groups) > 0) {
        for (i in 1:length(groups)) {
          group_rgx <- glue("\\b{groups[i]}\\b")
          group_indices <- str_detect(object$survey$group, group_rgx)
          group_vars <- object$survey$name[group_indices]
          vars <- append(vars, group_vars)
          relevants <- append(relevants, rep(groups_rel[i], length(group_vars)))
        }
      }

      indices <- match(vars, object$survey$name)
      sheets <- object$survey$sheet[indices]
      original_sheet <- filter(object$survey, name == q_name)$sheet

      # Separating out relevants on separate sheets
      nested_sheets <- sheets[!(sheets == original_sheet)]
      nested_vars <- vars[!(sheets == original_sheet)]
      nested_rel <- relevants[!(sheets == original_sheet)]

      # Relevants on the same sheet
      same_vars <- vars[sheets == original_sheet]
      same_relevants <- relevants[sheets == original_sheet]

      # Mapping the two different relevant update functions
      pmap(list(
        original_sheet,
        nested_sheets,
        nested_vars,
        nested_rel,
        chg_uuid,
        chg_index,
        chg_relevant
      ), separate_relevants)

      pmap(list(
        original_sheet,
        same_vars,
        same_relevants,
        chg_uuid,
        chg_index,
        chg_relevant
      ), same_relevants)

    }
  }

  # General cleaning function ------------------------------------------------------------------------------

  general_cleaner <- function(type, name, value, sheet, uuid, index, relevant) {
    if (type == "change_response") {
      change_response(
        name,
        value,
        uuid,
        relevant
      )
    } else if (type == "remove_survey") {
      remove_entry(
        "data",
        uuid,
        index,
        relevant
      )
    } else if (type == "remove_loop_entry") {
      remove_entry(
        sheet,
        uuid,
        index,
        relevant
      )
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
    relevant_updater(name, uuid, index, relevant)


  }

  pmap(
    list(
      object$cleaning$type,
      object$cleaning$name,
      object$cleaning$value,
      object$cleaning$sheet,
      object$cleaning$uuid,
      object$cleaning$index,
      object$cleaning$relevant
    ),
    general_cleaner
  )

  return(object)
}
