## Functions below convert skip logic (relevant) coding from XLS Form into R strings.
## Once the entire string is converted, it's passed through rlang::parse_expr() into
## a filter and evaluated to, mimic the working of XLS Form coding.

#' @title Interpret selected function from XLS Form
#' @description `convert_selected` converts selected(var, val) into R language of the format
#' var %in% 'val'.
#'
#' @details Returns a string value of the format var %in% 'val', which can be
#' parsed for use in filtering datasets. Works together with other internal functions
#' to fully intrepet relevant logic strings from XLS Form coding in `convert_relevant`.
#'
#' @param string String to be interpreted, should be of the format "selected(${var}, val)"
#'
#' @importFrom stringr str_match
#'
convert_selected <- function(string) {
   var <- var_extract(string)
   answer <- str_match(string, "(?:\'|\")(.*)(?:\'|\")")[2]
   paste0(var, " %in% '", answer, "'")
}

#' @title Extract variable names from XLS Form
#' @description `convert_selected` extracts a variable from the XLS Form
#' format of ${var} and removes the bracketing.
#'
#' @details Returns a string value of the variable, which can be parsed for use in filtering datasets.
#' Works together with other internal functions to fully interpret relevant logic string from XLS Form
#' coding in `convert_relevant`.
#'
#' @param string String to be interpreted, should be of the format "${var}"
#'
#' @importFrom stringr str_match
var_extract <- function(string) {
   str_match(string, "\\$\\{(.*?)\\}")[,2]
}

#' @title Interpret count-selected function from XLS Form
#' @description `convert_countsel` converts count-selected(${var}) into R langage of the format
#' str_count(var, " ").
#'
#' @details Takes in a string of the format count-selected(${var}) and returns a string
#' value of the format str_count(var, ' '), which can be #' parsed for use in filtering
#' datasets. Works together with other internal functions #' to fully intrepet relevant
#' logic strings from XLS Form coding in `convert_relevant`.
#'
#' @param string String to be interpreted, should be of the format "count-selected(${var})"
#'
#' @importFrom stringr str_count
convert_countsel <- function(string) {
   var <- var_extract(string)
   paste0("str_count(", var, ", ' ')")
}

## Final interpreter, which will convert the relevant string into R logic, using functions above
#' @title Interpret relevant logic from XLS Form
#' @description `convert_relevant` converts relevant relevant logic from XLS Form into R language.
#'
#' @details Takes in a string of relevant logic from XLS Form and converts all portions of it into returns a parsed expression that
#' can be passed to `dplyr::filter` and used to filter data frames.
#'
#' @param string String to be interpreted, should be of the standard format for relevant logic within XLS Forms.
#'
#' @importFrom stringr str_replace_all
#' @importFrom rlang parse_expr
#' @export
convert_relevant <- function(string) {
   string <- str_replace_all(string, "count-selected\\((.*?)\\)", convert_countsel)
   string <- str_replace_all(string, "selected\\((.*?)\\)", convert_selected)
   string <- str_replace_all(string, c("and" = "&", "or" = "|", "not" = "!", "=" = "=="))
   string <- str_replace_all(string, "\\$\\{(.*?)\\}", var_extract)
   string <- parse_expr(string)
   return(string)
}
