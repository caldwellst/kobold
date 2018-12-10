#' @title Load XLS Form file
#'
#' @description \code{read_xls_form} loads an XLS Form and its accompanying data into R.
#'
#' @details This function loads XLS Form Excel files into R, while also allowing additional sheets for exported data and cleaning.
#'   It's loaded as an S3 class \code{kobold}, and can be worked with using functions within this package, or the data can
#'   be retrieved as a data frame or tibble using generics. The columns of the data are given a class according to the
#'   survey and choices sheet.
#'
#' @param filepath Path to the XLS Form
#' @param data Name of the data worksheet, Default: 'data'
#' @param cleaning Name of the cleaning worksheet, Default: 'cleaning'
#' @param survey Name of the survey worksheet, Default: 'survey'
#' @param choices Name of the choices worksheet, optional, Default: 'choices';
#'
#' @return Returns a \code{kobold} class object, a modified list containing the above data frames.
#'
#' @importFrom readxl excel_sheets read_excel
#' @importFrom stringr str_remove_all str_detect str_match str_c
#' @importFrom dplyr mutate_all select matches one_of mutate_at vars filter
#' @importFrom lubridate as_datetime
#' @importFrom magrittr %>%
#' @importFrom purrr map
#' @importFrom tidyr replace_na
#' @importFrom glue glue_collapse glue
#'
#' @export
read_xls_form <- function(filepath,
                          data = "data",
                          cleaning = "cleaning",
                          survey = "survey",
                          choices = "choices") {
  # Select only the sheets of the file that we need
  worksheets <- excel_sheets(filepath)

  # Loading the Excel sheets into a new class so we can make all names of the list objects the same
  object <- suppressWarnings(suppressMessages(new_kobold(
    data = read_excel(filepath, data),
    cleaning = read_excel(filepath, cleaning),
    survey = read_excel(filepath, survey),
    choices = read_excel(filepath, choices)))
  )

  # Load in repeat group data (if available)

  rep_reg <- "^.*(begin_repeat|begin repeat)"
  rep_rows <- filter(object$survey,
                       str_detect(type, rep_reg))
  rep_groups <- rep_rows$name

  rep_sheets <- worksheets[worksheets %in% rep_groups]
  rep_missing <- rep_groups[!(rep_groups %in% worksheets)]

  if(length(rep_missing) > 0) {
    rep_missing <- glue_collapse(rep_missing, sep = ", ")
    warning(
      glue("Repeat group worksheets {rep_missing} were not found.")
    )
  }

  # Function to load in extra worksheets

  load_sheet <- function(sheet) {
    suppressWarnings(suppressMessages(
      object[[sheet]] <<- read_excel(filepath, sheet)
    ))
  }

  map(rep_sheets, load_sheet)

  # Isolating sheet names with data to be cleaned/worked with
  data_sheets <- c("data", rep_sheets)

  # Creating list_name column for the survey sheet, for easy linkages to the choices sheet.
  # First we create new column removing all of the "type" values for selects, and removing blanks.
  # Then we make the value empty for a row that DIDN'T have a select question. So all that is left
  # is a character vector containing the list_names for selects!

  object$survey$list_name <- str_remove_all(
    object$survey$type, "^.*(select_multiple|select multiple| |select_one|select one)")
  object$survey$list_name[!str_detect(
    object$survey$type,
    "^.*(select_multiple|select multiple|select_one|select one)"
  )] <- ""

  # Function for converting columns to the proper type within the data sheet of the kobold object.
  # Uses R's scoping assignment to change the kobold variable in the broader function environment.
  convert_columns <- function(sheet, types, converter) {
    types <- str_c(types, collapse = "|")
    types <- str_c("^(?!.*select).*(", types, ").*")

    name_rows <- filter(object$survey,
                    str_detect(type, types))
    cnv_names <- name_rows$name
    sht_names <- names(object[[sheet]])
    cols <- unique(cnv_names[cnv_names %in% sht_names])
    suppressWarnings(suppressMessages(object[[sheet]] <<-
                                        object[[sheet]] %>% mutate_at(vars(
                                          one_of(cols)
                                        ), converter)))
  }

  # Function to convert columns of select_multiple individual options to logical vectors.
  # Uses the same scoping as the previous function. Have to convert select_multiple functions
  # first to numeric vectors (since they often arrive as "0" "1" character vectors) and then
  # convert to logical vectors. Most of the code here is spent trying to locate the columns
  # corresponding to the possible choices.
  convert_select_multiple <- function(sheet) {
    sel_mul_reg <- "^.*(select_multiple|select multiple)"
    list_rows <- filter(object$survey, str_detect(type, sel_mul_reg))
    lists <- list_rows$list_name
    lists <- str_c(lists, collapse = "|")
    lists_reg <- glue("^.*({lists})s")

    choice_rows <- filter(object$choices, str_detect(list_name, lists_reg))
    choices <- choice_rows$name
    choices <- str_c(choices, collapse = "|")
    choices_reg <- str_c("(", choices, ")$")

    name_rows <- filter(object$survey, str_detect(type, sel_mul_reg))
    names <- name_rows$name
    names <- str_c(names, collapse = "|")
    names_reg <- str_c("^(", names, ")")

    survey_names <- object$survey$name

    suppressWarnings(suppressMessages(
      retype_cols <- object[[sheet]] %>%
        select(-one_of(survey_names)) %>%
        select(matches(names_reg)) %>%
        select(matches(choices_reg))
    ))

    retype_names <- unique(names(retype_cols))

    log_num <- function(x) {
      as.logical(as.numeric(x))
    }

    object[[sheet]][retype_names] <<-
      mutate_all(object[[sheet]][retype_names], log_num)

  }

  # Converting the columns for each type
  map(data_sheets,
      convert_columns,
      c("decimal", "integer", "range"),
      as.numeric)

  map(data_sheets,
      convert_columns,
      c("start", "end", "today", "date", "time", "dateTime"),
      as_datetime)

  map(data_sheets,
      convert_select_multiple)

  return(object)
}
