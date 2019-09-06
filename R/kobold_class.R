#' Create a kobold object from data frames
#'
#' @description Constructor of a kobold list using XLSForm survey/choices and other possible data frames:
#' \itemize{
#' \item Main data sheet
#' \item Loop data sheet(s), which do not need to be explicitly loaded
#' \item Cleaning sheet
#' }
#'
#' @param survey Data frame containing XLSForm survey.
#' @param choices Data frame containing XLSForm choices, optional.
#' @param data Data frame containing main XLSForm data export, optional.
#' @param cleaning Data frame containing cleaning sheet for use with kobold, optional.
#' @param ... Data frame(s) for repeat group data; names must match repeat_group names from \code{survey}, optional.
#'
#' @importFrom dplyr filter
#' @importFrom glue glue_collapse glue
#' @importFrom stringr str_detect
#' @importFrom rlang current_env
#' @importFrom tibble tibble
#' @importFrom purrr map
#'
#' @export
kobold <- function(survey, choices = NULL, data = NULL, cleaning = NULL, ...) {
  object <- new_kobold(data, cleaning, survey, choices)

  env <- current_env()

  # Identify repeat group data.frames ------------------------------------------

  ellipsis_sheets <- ellipsis_names(...)
  ellipsis_data <- setNames(list(...), ellipsis_sheets)

  rep_reg <- "^.*(begin_repeat|begin repeat)"
  rep_rows <- filter(object$survey,
                     str_detect(type, rep_reg))
  rep_groups <- rep_rows$name
  rep_parents <- rep_rows$parent
  rep_parents[rep_parents == ""] <- "data"

  rep_sheets <- ellipsis_sheets[ellipsis_sheets %in% rep_groups]
  rep_parents <- rep_parents[rep_groups %in% rep_sheets]

  rep_missing <- rep_groups[!(rep_groups %in% ellipsis_sheets)]

  if(length(rep_missing) > 0) {
    rep_missing <- glue_collapse(rep_missing, sep = ", ")
    warn(
      glue("Repeat group worksheets {rep_missing} were not found.")
    )
  }

  # Append in repeat group data frames ----------------------------------------

  object <- append(object, ellipsis_data[rep_sheets])

  # Isolating sheet names with data to be cleaned/worked with ------------------
  data_sheets <- tibble(sheets = c("data", rep_sheets),
                        parent = c("", rep_parents))

  object$data_sheets <- data_sheets

  # Rename UUID, index, and parent_index columns -------------------------------
  map(data_sheets$sheets,
      rename_column,
      "uuid",
      "^.*(_uuid\\b)",
      env
  )

  map(data_sheets$sheets,
      rename_column,
      "index",
      "^(.)?(_index\\b)",
      env)

  if (length(data_sheets$sheets) > 1) {
    map(data_sheets$sheets[-1],
        rename_column,
        "parent_index",
        "^(.)?(_parent_index\\b)",
        env
    )
  }

  # Converting the columns according to type -----------------------------------
  class_converter(env)

  return(object)
}

# Defining generics for kobold class objects

#' as.data.frame generic for kobold
#'
#' @export
as.data.frame.kobold <- function(df) {
  as.data.frame(df[["data"]])
}

#' as_tibble generic for kobold
#'
#' @importFrom tibble as_tibble
#'
#' @export
as_tibble.kobold <- function(df) {
  df[["data"]]
}

#' Generic kobold generator
#'
#' @importFrom tibble as_tibble
new_kobold <- function(survey, choices, data, cleaning) {
  if (!is.null(choices)) {
    choices <- as_tibble(choices)
  }

  if (!is.null(data)) {
    data <- as_tibble(data)
  }

  if (!is.null(cleaning)) {
    cleaning <- as_tibble(cleaning)
  }

  object <- list(
    survey = as_tibble(survey),
    choices = choices,
    data = data,
    cleaning = cleaning
  )

  env <- current_env()

  # Identifying loops and groups for each question -----------------------------
  identify_groups(env)

  # Identify list name for select questions ------------------------------------
  identify_list_name(env)

  class(object) <- c("kobold", class(object))
}
