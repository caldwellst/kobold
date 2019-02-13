#' Function to update relevant logic referencing a different sheet
#'
#' @importFrom glue glue glue_collapse
#' @importFrom dplyr filter select matches mutate
#' @importFrom rlang sym
separate_relevants <- function(original_sheet, sheet, q_name, relevant, env) {
  select_multiple <- str_detect(c(filter(env$object$survey, name == q_name)$type), "^.*(select_multiple|select multiple)")
  if (select_multiple) {
    l_name <- filter(env$object$survey, name == q_name)$list_name
    choices <- filter(env$object$choices, list_name == l_name)$name
    search_rgx <- glue("(\\b{q_name})(\\.|\\/)({choices}\\b)")
    search_rgx <- glue_collapse(search_rgx, sep = "|")
    binary_names <- unique(names(env$object[[sheet]] %>%
                                   select(matches(search_rgx))))
  }

  # Get the UUID from the main sheet to connect to separate sheets
  if (!is.na(match("uuid", names(env$object[[sheet]])))) {
    chg_uuid <- filter(env$object[[original_sheet]], !(!!convert_relevant(relevant)))$uuid
    env$object[[sheet]] <<- mutate(env$object[[sheet]],
                               !!q_name := ifelse(uuid == chg_uuid,
                                                  NA,
                                                  !!sym(q_name)))
    if (select_multiple) {
      for (i in 1:length(binary_names)) {
        env$object[[sheet]] <<- mutate(env$object[[sheet]],
                                   !!binary_names[i] := ifelse(uuid == chg_uuid,
                                                               NA,
                                                               !!sym(binary_names[i])))
      }
    }
  } else if (!is.na(match("index", names(object[[sheet]])))) {
    sheet_chain <- filter(env$object$data_sheets, sheets == sheet)$parent

    while (sheet_chain[1] != original_sheet) {
      parent <- filter(env$object$data_sheets, sheets == sheet_chain[1])$parent
      sheet_chain <- append(sheet_chain, parent, before = 0)
    }

    sheet_chain <- append(sheet_chain, sheet)
    chg_index <- filter(env$object[[original_sheet]], !(!!convert_relevant(relevant)))$index

    for (i in 2:length(sheet_chain)) {
      chg_index <- filter(object[[sheet_chain[i]]], parent_index %in% chg_index)$index
    }

    env$object[[sheet]] <<- mutate(env$object[[sheet]],
                               !!q_name := ifelse(index == chg_index,
                                                  NA,
                                                  !!sym(q_name)))
    if (select_multiple) {
      for (i in 1:length(binary_names)) {
        env$object[[sheet]] <<- mutate(env$object[[sheet]],
                                   !!binary_names[i] := ifelse(index == chg_index,
                                                               NA,
                                                               !!sym(binary_names[i])))
      }
    }
  }
}

#' Function to update relevant logic referencing the same sheet
#'
#' @importFrom dplyr filter mutate select matches %>%
#' @importFrom glue glue glue_collapse
#' @importFrom rlang sym
same_relevants <- function(sheet, q_name, relevant, env) {
  select_multiple <- str_detect(c(filter(env$object$survey, name == q_name)$type), "^.*(select_multiple|select multiple)")
  if (select_multiple) {
    l_name <- filter(env$object$survey, name == q_name)$list_name
    choices <- filter(env$object$choices, list_name == l_name)$name
    search_rgx <- glue("(\\b{q_name})(\\.|\\/)({choices}\\b)")
    search_rgx <- glue_collapse(search_rgx, sep = "|")
    binary_names <- unique(names(env$object[[sheet]] %>%
                                   select(matches(search_rgx))))
  }

  env$object[[sheet]] <- mutate(env$object[[sheet]],
                                !!q_name := ifelse(!(!!convert_relevant(relevant)),
                                                   NA,
                                                   !!sym(q_name)))
  if (select_multiple) {
    for (i in 1:length(binary_names)) {
      env$object[[sheet]] <- mutate(env$object[[sheet]],
                                    !!binary_names[i] := ifelse(!(!!convert_relevant(relevant)),
                                                                NA,
                                                                !!sym(binary_names[i])))
    }
  }
}

#' Function to update relevant logic
#'
#' @importFrom glue glue
#' @importFrom stringr str_which str_detect
#' @importFrom dplyr filter
#' @importFrom purrr pmap map2
relevant_updater <- function(q_name, type, relevant, env) {

  group_rgx <- "^.*(begin_group|begin group|begin repeat|begin_repeat)"
  group <- str_detect(type, group_rgx)

  if (group) {
    group_rgx <- glue("\\b{q_name}\\b")
    indices <- str_detect(object$survey$group, group_rgx)
    vars <- object$survey$name[indices]
    types <- object$survey$type[indices]
    map2(vars, types, relevant_updater, relevant, env)
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
