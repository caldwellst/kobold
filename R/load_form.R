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
#' @importFrom stringr str_detect
#' @importFrom dplyr filter
#' @importFrom purrr map
#' @importFrom glue glue_collapse glue
#' @importFrom rlang warn current_env
#' @importFrom tibble tibble
#'
#' @export
read_xls_form <- function(filepath,
                          data = "data",
                          cleaning = "cleaning",
                          survey = "survey",
                          choices = "choices") {

  env <- current_env()

  worksheets <- excel_sheets(filepath)

  # Loadingsheets into kobold class object -------------------------------------
  object <- suppressWarnings(suppressMessages(new_kobold(
    read_excel(filepath, data, guess_max = Inf),
    read_excel(filepath, cleaning, col_types = "text"),
    read_excel(filepath, survey, col_types = "text"),
    read_excel(filepath, choices, col_types = "text")))
  )

  # Identifying loops and groups for each question -----------------------------
  identify_groups(env)

  # Identify list name for select questions ------------------------------------
  identify_list_name(env)

  # Identify repeat group worksheets -------------------------------------------

  rep_reg <- "^.*(begin_repeat|begin repeat)"
  rep_rows <- filter(object$survey,
                     str_detect(type, rep_reg))
  rep_groups <- rep_rows$name
  rep_parents <- rep_rows$parent
  rep_parents[rep_parents == ""] <- "data"

  rep_sheets <- worksheets[worksheets %in% rep_groups]
  rep_parents <- rep_parents[rep_groups %in% rep_sheets]

  rep_missing <- rep_groups[!(rep_groups %in% worksheets)]

  if(length(rep_missing) > 0) {
    rep_missing <- glue_collapse(rep_missing, sep = ", ")
    warn(
      glue("Repeat group worksheets {rep_missing} were not found.")
    )
  }

  # Load in repeat group worksheets --------------------------------------------

  load_sheet <- function(sheet) {
    suppressWarnings(suppressMessages(
      object[[sheet]] <<- read_excel(filepath, sheet, guess_max = Inf)
    ))
  }

  map(rep_sheets, load_sheet)

  # Isolating sheet names with data to be cleaned/worked with
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
