#' @title Interpret selected function from XLS Form
#' @description `convert_selected` converts selected(var, val) into an equivalent R statement.
#'
#' @return String value of the format var %in% 'val', which can be
#'   parsed for use in filtering datasets. Works together with other internal functions
#'   to fully intrepet relevant logic strings from XLS Form coding in `convert_relevant`.
#'
#' @param string String to be interpreted, should be of the format "selected(${var}, val)"
#'
#' @examples convert_selected("selected(${refugee_origin}, 'hoth')")
#'
#' @importFrom stringr str_match
#' @importFrom glue glue
#'
convert_selected <- function(string) {
   var <- var_extract(string)
   answer <- str_match(string, "(?:\'|\")(.*)(?:\'|\")")[2]
   paste0("grepl('\\\\b", answer, "\\\\b',", var,")")
}

#' @title Extract variable names from XLS Form
#' @description `convert_selected` extracts the variable from the XLS Form
#'   format of ${variable}.
#'
#' @return String value of the variable, which can be parsed for use in filtering datasets.
#'   Works together with other internal functions to fully interpret relevant logic string from XLS Form
#'   coding in `convert_relevant` and `convert_calculate`.
#'
#' @param string String to be interpreted, should be of the format "${var}"
#'
#' @examples var_extract("${supports_rebels}")
#'
#' @importFrom stringr str_match
var_extract <- function(string) {
   str_match(string, "\\$\\{(.*?)\\}")[,2]
}

#' @title Interpret count-selected function from XLS Form
#' @description `convert_count_selected` converts count-selected(${var}) into an equivalent R statement.
#'
#' @return Returns a string
#' value of the format str_count(var, ' '), which can be #' parsed for use in filtering
#' datasets. Works together with other internal functions #' to fully intrepet relevant
#' logic strings from XLS Form coding in `convert_relevant` and `convert_calculate`.
#'
#' @param string String to be interpreted, should be of the format "count-selected(${var})"
#'
#' @examples convert_count_selected("count-selected(${livelihoods})")
#'
#' @importFrom stringr str_count
convert_count_selected <- function(string) {
   var <- var_extract(string)
   paste0("str_count(", var, ", ' ')")
}

#' @title Interpret sum-at function from XLS Form
#' @description `convert_sum` converts sum(${var}) into an equivalent R statement.
#'
#' @return Returns a string
#' value of the format str_count(var, ' '), which can be #' parsed for use in filtering
#' datasets. Works together with other internal functions #' to fully intrepet relevant
#' logic strings from XLS Form coding in `convert_relevant` and `convert_calculate`.
#'
#' @param string String to be interpreted, should be of the format "count-selected(${var})"
#'
#' @examples sum(${hh_member}))
#'
#' @importFrom stringr str_match
convert_sum <- function(string) {
  var <- var_extract(string)
  check <- paste0("kobold_sum(", var, ")")
}

#' @title Interpret selected-at function from XLS Form
#' @description `convert_selected_at` converts selected-at(${var}, n) into an equivalent R statement.
#'
#' @return Returns a string
#' value of the format str_count(var, ' '), which can be #' parsed for use in filtering
#' datasets. Works together with other internal functions #' to fully intrepet relevant
#' logic strings from XLS Form coding in `convert_calculate`.
#'
#' @param string String to be interpreted, should be of the format "count-selected(${var})"
#'
#' @examples convert_selected_at("selected_at(${biggest_needs}, 1)")
#'
#' @importFrom stringr str_match
convert_selected_at <- function(string) {
  var <- var_extract(string)
  n <- as.integer(str_match(string, "(?:\\, |\\,)(\\d*)")[2]) + 1
  check <- paste0("map_chr(str_split(", var, ", pattern = ' '), ", n, ")")
}

#' @title Interpret relevant logic from XLS Form
#' @description \code{convert_relevant} converts relevant logic from XLS Form into an R language equivalent.
#'
#' @details Takes in a string of relevant logic from XLS Form and converts all portions of it into a parsed expression that
#'   can be passed to \code{dplyr::filter} for use in filtering data frames.
#'
#' @param string String to be interpreted, should be of the standard format for relevant logic within XLS Forms.
#'
#' @importFrom stringr str_replace_all
#' @importFrom rlang parse_expr
#'
#' @examples
#' convert_relevant("selected(${employed_by_empire}, 'yes')")
#' convert_relevant("selected(${employed_by_empire}, 'yes') and count-selected(${livelihoods_coping}) > 3")
#'
#' @export
convert_relevant <- function(string) {
   string <- str_replace_all(string, "count-selected\\((.*?)\\)", convert_count_selected)
   string <- str_replace_all(string, "selected-at\\((.*?)\\)", convert_selected_at)
   string <- str_replace_all(string, "selected\\((.*?)\\)", convert_selected)
   string <- str_replace_all(string, c("\\band\\b" = "&",
                                       "\\bor\\b" = "|",
                                       "\\bnot\\b" = "!",
                                       "=" = "=="))
   string <- str_replace_all(string, "\\$\\{(.*?)\\}", var_extract)
   relevant_expr <- parse_expr(string)
   return(relevant_expr)
}

#' @title Interpret calculations  from XLS Form
#' @description \code{convert_relevant} converts calculations logic from XLS Form into an R language equivalent.
#'
#' @details Takes in a string of calculations from XLS Form and converts all portions of it into a parsed expression that
#'   can be passed to \code{dplyr::filter} for use in recalculating data after cleaning.
#'
#' @param string String to be interpreted, should be of the standard format for calculate logic within XLS Forms.
#'
#' @importFrom stringr str_replace_all
#' @importFrom rlang parse_expr
#'
#' @export
convert_calc <- function(string) {
  string <- str_replace_all(string, "count-selected\\((.*?)\\)", convert_count_selected)
  string <- str_replace_all(string, "selected-at\\((.*?)\\)", convert_selected_at)
  string <- str_replace_all(string, "selected\\((.*?)\\)", convert_selected)
  string <- str_replace_all(string, c("\\band\\b" = "&",
                                      "\\bor\\b" = "|",
                                      "\\bnot\\b" = "!",
                                      "=" = "==",
                                      "if" = "ifelse",
                                      "div" = "/",
                                      "mod" = "%%"))
  string <- str_replace_all(string, "\\$\\{(.*?)\\}", var_extract)
  relevant_expr <- parse_expr(string)
  return(relevant_expr)
}
