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

#' Interpret count-selected function from XLS Form
#'
#' `convert_count_selected` converts count-selected(${var}) into an equivalent R statement.
#'
#' @return Returns a string
#'   value of the format `str_count(var, ' ')`, which can be parsed for use in filtering
#'   datasets. Works together with other internal functions to fully intrepet relevant
#'   logic strings from XLS Form coding in `convert_relevant()` and `convert_calculate()`.
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

#' Interpret sum-at function from XLS Form
#'
#' `convert_sum` converts sum(${var}) into an equivalent R statement.
#'
#' @return Returns a string
#' value of the format kobold_sum(var). Works together with other internal functions to fully intrepet calculate
#' strings from XLS Form coding in `convert_calculate`.
#'
#' @param string String to be interpreted, should be of the format "sum(${var})"
#'
#' @examples sum(${hh_member})
#'
#' @noRd
convert_sum <- function(string) {
  var <- var_extract(string)
  paste0("kobold_sum(", var, ")")
}

#' Interpret coalesce function from XLS Form
#'
#' `convert_coalesce` converts coalesce(${var}, n) into an equivalent R statement.
#'
#' @return Returns a string
#' value of the format coalesce(var, ' '). Works together with other internal functions to fully intrepet relevant
#' logic strings from XLS Form coding in `convert_calculate`.
#'
#' @param string String to be interpreted, should be of the format "coalesce(${var}, n)"
#'
#' @examples `coalesce(${male_3_5}, 0)`
#'
#' @importFrom stringr str_match
#'
#' @noRd
convert_coalesce <- function(string) {
  var <- var_extract(string)
  n <- as.integer(str_match(string, "(?:\\, |\\,)(\\d*)")[2])
  paste0("ifelse(is.na(", var, "), ", n, ", ", var,")")
}

#' Interpret selected-at function from XLS Form
#'
#' `convert_selected_at` converts selected-at(${var}, n) into an equivalent R statement.
#'
#' @return Returns a string
#' value of the format str_count(var, ' '), which can be parsed for use in filtering
#' datasets. Works together with other internal functions to fully intrepet strings from
#' XLS Form coding in `convert_relevant` and `convert_calculate`.
#'
#' @param string String to be interpreted, should be of the format "count-selected(${var})"
#'
#' @examples convert_selected_at("selected_at(${biggest_needs}, 1)")
#'
#' @importFrom stringr str_split str_match
#' @importFrom purrr map_chr
convert_selected_at <- function(string) {
  var <- var_extract(string)
  n <- as.integer(str_match(string, "(?:\\, |\\,)(\\d*)")[2]) + 1
  paste0("map_chr(str_split(", var, ", pattern = ' '), ", n, ")")
}

#' Interpret logicals involving blanks
#'
#' `convert_logical_blanks` converts logical statements involving blanks into equivalent R statements using is.na.
#'
#' @return Returns a string
#' value of the format `is.na(var)` or `!is.na(var)`. Works together with other internal functions to fully intrepet calculate
#' strings from XLS Form coding in `convert_calculate`.
#'
#' @param string String to be interpreted, should be of the format "convert_logical_blanks(${var} = '')"
#'
#' @examples `convert_logical_blanks("${hh_needs} = ''")`
#'
#' @importFrom stringr str_split
#' @importFrom purrr map_chr
convert_logical_blanks <- function(string) {
  var <- var_extract(string)
  if (str_detect(string, "!=")) {
    paste0("!is.na(", var, ")")
  } else if (str_detect(string, "=")) {
    paste0("is.na(", var, ")")
  }
}

#' Parse quotations within XLS Form code
#' `parse_quotes` identifies parts of XLS Form strings enclosed by quotes.
#'
#' @details Takes in a string of relevant logic from XLS Form and locates string index for quote enclosures
#'
#' @param string String to be interpreted, should be of the standard format for relevant logic within XLS Forms.
#'
#' @importFrom stringr str_locate_all
#' @importFrom purrr map_dbl
#'
#' @noRd
parse_quotes <- function(string) {
  quotes <- list(str_locate_all(string, "\\'")[[1]][,2],
                 str_locate_all(string, '\\"')[[1]][,2])
  quotes <- lapply(quotes, as.numeric)
  quoted_portion <- list()
  while (length(quotes[[1]]) + length(quotes[[2]]) > 0) {
    if (length(quotes[[1]]) > 0 & length(quotes[[2]]) == 0) {
      quoted_portion <- append(quoted_portion, list(c(quotes[[1]])))
      break
    } else if (length(quotes[[2]]) > 0 & length(quotes[[1]]) == 0) {
      quoted_portion <- append(quoted_portion, list(c(quotes[[2]])))
      break
    } else {
      which_quote <- which.min(map_dbl(quotes, min))
      other_quote <- which.max(map_dbl(quotes, min))
      start <- quotes[[which_quote]][1]
      end <- quotes[[which_quote]][2]
      quoted_portion <- append(quoted_portion, list(c(start, end)))
      quotes[[which_quote]] <- quotes[[which_quote]][c(-1,-2)]
      quotes[[other_quote]] <- quotes[[other_quote]][quotes[[other_quote]] > end]
    }
  }
  return(quoted_portion)
}

#' Parse functions within XLS Form code
#' `parse_functions` identifies parts of XLS Form strings enclosed by quotes.
#'
#' @details Takes in a string of relevant logic from XLS Form and locates string index for quote enclosures
#'
#' @param string String to be interpreted, should be of the standard format for relevant logic within XLS Forms.
#'
#' @importFrom stringr str_locate_all
#' @importFrom purrr map_dbl
#'
#' @noRd
parse_functions <- function(string) {
  quotes <- parse_quotes(string)
  openers <- str_locate_all(string, "\\(")[[1]][,2]
  closers <- str_locate_all(string, "\\)")[[1]][,2]

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
#' @importFrom lubridate as_date
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
                                       "(?<!!|>|<)=" = "==",
                                       "decimal-date-time" = "as_date"))
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
  string <- str_replace_all(string, "sum\\((.*?)\\)", convert_sum)
  string <- str_replace_all(string, "coalesce\\((.*?)\\)", convert_coalesce)
  string <- str_replace_all(string, "jr\\:choice\\-name\\((.*?)\\)", convert_choice_name)
  string <- str_replace_all(string, c("\\band\\b" = "&",
                                      "\\bor\\b" = "|",
                                      "\\bnot\\b" = "!",
                                      "(?<!!)=" = "==",
                                      "if(" = "ifelse(",
                                      "div" = "/",
                                      "mod" = "%%",
                                      "concat" = "paste0",
                                      "int" = "floor",
                                      "number" = "as.numeric",
                                      "true()" = "TRUE",
                                      "false()" = "FALSE",
                                      "max(" = "kobold_max(",
                                      "min(" = "kobold_min(",
                                      "sum(" = "kobold_sum(",
                                      "random(" = "runif("))
  string <- str_replace_all(string, "\\$\\{(.*?)\\}", var_extract)
  relevant_expr <- parse_expr(string)
  return(relevant_exp)
}
