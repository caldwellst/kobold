#' Remove select_multiple value
#'
#' @importFrom glue glue
#' @importFrom stringr str_trim str_replace str_remove
select_mul_str_removal <- function(column, value) {
  option = glue("\\b{value}\\b")
  str_trim(str_replace(str_remove(column, option), "  ", " "))
}

#' Add select_multiple valeu
#'
#' @importFrom glue glue
#' @importFrom stringr str_split
#' @importFrom purrr map
select_mul_str_adder <- function(column, value, choices) {
  split <- str_split(column, pattern = " ")
  exist_test <- map_lgl(split, function(x)
    is.na(match(value, x)))
  split <- ifelse(exist_test, map(split, append, value), split)
  split <- map(split, function(x)
    x[order(match(x, choices))])
  map_chr(split, str_c, collapse = " ")
}
