#' Update relevant logic referencing a different sheet
#'
#' @importFrom glue glue glue_collapse
#' @importFrom dplyr filter select matches mutate
#' @importFrom rlang sym !! := is_empty
#' @importFrom stringr str_detect
#'
#' @noRd
separate_relevants <- function(rel_sheet, var_sheet, q_name, relevant, env) {
  select_multiple <- str_detect(c(filter(env$object$survey, name == q_name)$type), "^.*(select_multiple|select multiple)")
  if (select_multiple) {
    l_name <- filter(env$object$survey, name == q_name)$list_name
    choices <- filter(env$object$choices, list_name == l_name)$name
    search_rgx <- glue("(\\b{q_name})(\\.|\\/)({choices}\\b)")
    search_rgx <- glue_collapse(search_rgx, sep = "|")
    binary_names <- unique(names(env$object[[var_sheet]] %>%
                                   select(matches(search_rgx))))
  }

  # Get the UUID from the main sheet to connect to separate sheets
  if (!is.na(match("uuid", names(env$object[[var_sheet]])))) {
    chg_uuid <- filter(env$object[[rel_sheet]], !(!!convert_relevant(relevant)))$uuid
    if (!is_empty(chg_uuid)) {
      env$object[[var_sheet]] <- mutate(env$object[[var_sheet]],
                                        !!q_name := ifelse(uuid %in% chg_uuid,
                                                           NA,
                                                           !!sym(q_name)))
      if (select_multiple) {
        for (i in 1:length(binary_names)) {
          env$object[[var_sheet]] <- mutate(env$object[[var_sheet]],
                                            !!binary_names[i] := ifelse(uuid %in% chg_uuid,
                                                                        NA,
                                                                        !!sym(binary_names[i])))
        }
      }
    }
  } else if (!is.na(match("index", names(env$object[[var_sheet]])))) {
    sheet_chain <- filter(env$object$data_sheets, sheets == var_sheet)$parent

    while (sheet_chain[1] != rel_sheet) {
      parent <- filter(env$object$data_sheets, sheets == sheet_chain[1])$parent
      sheet_chain <- append(sheet_chain, parent, before = 0)
    }

    sheet_chain <- append(sheet_chain, var_sheet)
    chg_index <- filter(env$object[[rel_sheet]], !(!!convert_relevant(relevant)))$index
    i <- 2
    while (i <= length(sheet_chain)) {
      if (is_empty(chg_index)) {
        i <- length(sheet_chain) + 1
      } else {
        chg_index <- filter(env$object[[sheet_chain[i]]], parent_index %in% chg_index)$index
        i <- i + 1
      }
    }
    if (!is_empty(chg_index)) {
      env$object[[var_sheet]] <- mutate(env$object[[var_sheet]],
                                        !!q_name := ifelse(index %in% chg_index,
                                                           NA,
                                                           !!sym(q_name)))
      if (select_multiple) {
        for (i in 1:length(binary_names)) {
          env$object[[var_sheet]] <- mutate(env$object[[var_sheet]],
                                            !!binary_names[i] := ifelse(index %in% chg_index,
                                                                        NA,
                                                                        !!sym(binary_names[i])))
        }
      }
    }
  }
}

#' Update relevant logic referencing the same sheet
#'
#' @importFrom dplyr filter mutate select matches %>%
#' @importFrom glue glue glue_collapse
#' @importFrom rlang sym !! :=
#'
#' @noRd
same_relevants <- function(sheet, q_name, relevant, env) {
  select_multiple <- str_detect(c(filter(env$object$survey, name == q_name)$type), "^.*(select_multiple|select multiple)")
  if (select_multiple) {
    l_name <- filter(env$object$survey, name == q_name)$list_name
    choices <- filter(env$object$choices, list_name == l_name)$name
    search_rgx <- glue("(\\b{q_name})(\\.|\\/)({choices}\\b)")
    search_rgx <- glue_collapse(search_rgx, sep = "|")
    binary_names <- unique(names(env$object[[sheet]] %>%
                                   select(matches(search_rgx))))
    for (i in 1:length(binary_names)) {
      env$object[[sheet]] <- mutate(env$object[[sheet]],
                                    !!binary_names[i] := ifelse(!(!!convert_relevant(relevant)),
                                                                NA,
                                                                !!sym(binary_names[i])))
    }
  }

  env$object[[sheet]] <- mutate(env$object[[sheet]],
                                !!q_name := ifelse(!(!!convert_relevant(relevant)),
                                                   NA,
                                                   !!sym(q_name)))
}

#' Determine variable for relevant logic updating
#'
#' @importFrom glue glue
#' @importFrom stringr str_which str_detect str_match_all
#' @importFrom dplyr filter
#' @importFrom purrr pmap map2
#' @importFrom rlang warn
#'
#' @noRd
relevant_determiner <- function(q_name, type, relevant, env) {
  group_rgx <- "^.*(begin_group|begin group|begin repeat|begin_repeat)"
  group <- str_detect(type, group_rgx)

  # Ensure variables within gropus get relevants ------------
  if (group) {
    group_name <- glue("\\b{q_name}\\b")
    indices <- str_detect(env$object$survey$group, group_name) & !str_detect(env$object$survey$type, group_rgx)
    vars <- env$object$survey$name[indices]
    types <- env$object$survey$type[indices]
    types <- types[!is.na(vars)]
    vars <- vars[!is.na(vars)]
    map2(vars, types, relevant_determiner, relevant, env)
  } else {
    srch_term <- "\\$\\{(.*?)\\}"
    relevant_vars <- str_match_all(relevant, srch_term)[[1]][,2]
    relevant_vars <- unique(relevant_vars)

    rel_indices <- match(relevant_vars, env$object$survey$name)
    rel_sheets <- env$object$survey$sheet[rel_indices]
    rel_sheets <- unique(rel_sheets)
    var_sheet <- filter(env$object$survey, name == q_name)$sheet
    if (length(rel_sheets) > 1) {
      warn(glue("Can't correct for {q_name} relevant logic since it references two or more data sheets."))
    } else if (var_sheet == rel_sheets) {
      same_relevants(var_sheet, q_name, relevant, env)
    } else {
      separate_relevants(rel_sheets, var_sheet, q_name, relevant, env)
    }
  }
}

#' Update data based on XLSForm relevant logic
#'
#' @importFrom rlang current_env
#' @importFrom purrr pmap
#' @importFrom dplyr filter
#'
#'
relevant_updater <- function(object) {
  env <- current_env()
  relevant_data <- filter(object$survey,
                          !is.na(relevant))
  pmap(list(relevant_data$name,
            relevant_data$type,
            relevant_data$relevant),
       relevant_determiner,
       env)
  return(object)
}
