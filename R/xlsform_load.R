#' @title Load XLS Form file
#'
#' @description \code{read_xls_form} loads an XLS Form and its accompanying data into R.
#'
#' @details This function loads XLS Form Excel files into R, while also allowing additional sheets for exported data and cleaning.
#' It's loaded as an S3 class \code{kobold}, and can be worked with using functions within this package, or the data can
#' be retrieved as a data frame or tibble using generics. The columns of the data are given a class according to the
#' survey and choices sheet.
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
#' @importFrom stringr str_remove_all str_detect str_match
#' @importFrom dplyr mutate_all select
#' @importFrom tidyselect matches
#' @importFrom lubridate as_datetime
#' @importFrom magritter %>%
#'
#' @export
read_xls_form <- function(filepath,
                         data = "data",
                         cleaning = "cleaning",
                         survey = "survey",
                         choices = "choices") {
   ## Select only the sheets of the file that we need
   worksheets <- excel_sheets(filepath)
   worksheets <- worksheets[worksheets %in% c(choices, survey, data, cleaning)]

   ## Loading the Excel sheets into a new class so we can make all names of the list objects the same
   object <- new_kobold(data = read_excel(filepath, sheet = data, col_types = "text"),
                        cleaning = read_excel(filepath, sheet = cleaning),
                        survey = read_excel(filepath, sheet = survey),
                        choices = read_excel(filepath, sheet = choices))

   ## Creating list_name column for the survey sheet, for easy linkages to the choices sheet

   object$survey$list_name <- str_remove_all(object$survey$type, "^.*(select_multiple|select multiple| |select_one|select one)")
   object$survey$list_name[!str_detect(object$survey$type, "^.*(select_multiple|select multiple|select_one|select one)")] <- ""

   ## Function for converting columns to the proper type within the data sheet of the kobold object
   convert_columns <- function(types, converter) {
      retype_names <- c(subset(object$survey, str_detect(type, types))$name)
      retype_cols <- unique(retype_names[retype_names %in% names(object$data)])
      suppressWarnings(suppressMessages(
         object$data[retype_cols] <<- mutate_all(object$data[retype_cols], converter)
      ))
   }

   ## Converting the columns for each type
   convert_columns(c("decimal", "integer", "range"), as.numeric)
   convert_columns(c("start", "end", "today", "date", "time", "dateTime"), as_datetime)

   ## Function to convert columns of select_multiple individual options to logical vectors
   convert_select_multiple <- function() {
      retype_lists <- c(subset(object$survey, str_detect(type, "^.*(select_multiple|select multiple)"))$list_name)
      retype_lists <- paste0("^.*(", paste(retype_lists, collapse = "|"), ")s")
      retype_choice_names <- c(subset(object$choices, str_detect(list_name, retype_lists))$name)
      retype_choice_names <- paste0("(", paste(retype_choice_names, collapse = "|"), ")$")
      retype_names <- c(subset(object$survey, str_detect(type, "^.*(select_multiple|select multiple)"))$name)
      retype_names <- paste0("^(", paste(retype_names, collapse = "|"), ")")
      retype_cols <- unique(names(object$data %>%
                                     select(matches(retype_names)) %>%
                                     select(matches(retype_choice_names))))
      suppressWarnings(suppressMessages(
         object$data[retype_cols] <<- mutate_all(object$data[retype_cols], function(x) as.logical(as.numeric(x)))
      ))
   }

   convert_select_multiple()

   ## And return the list!
   return(object)
}
