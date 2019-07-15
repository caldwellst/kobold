#' Remove select_multiple value
#'
#' @importFrom glue glue
#' @importFrom stringr str_trim str_replace str_remove str_c
#'
#' @noRd
select_mul_str_removal <- function(column, value) {
  option = glue("\\b{value}\\b")
  str_trim(str_replace(str_remove(column, option), "  ", " "))
}

#' Add select_multiple value
#'
#' @importFrom glue glue
#' @importFrom stringr str_split str_c
#' @importFrom purrr map map_chr map_lgl
#'
#' @noRd
select_mul_str_adder <- function(column, value, choices) {
  split <- str_split(column, pattern = " ")
  exist_test <- map_lgl(split, function(x)
    is.na(match(value, x)))
  split <- ifelse(exist_test, map(split, append, value), split)
  split <- map(split, function(x)
    x[order(match(x, choices))])
  map_chr(split, function(x) str_c(x[!is.na(x)], collapse = " "))
}
