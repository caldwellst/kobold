#' Create a kobold list from an XLS Form
#'
#' @description Constructor of a kobold list using XLSForm survey and choices and other possible sheets:
#' * Main data sheet
#' * Loop data sheet(s), which do not need to be explicitly loaded
#' * Cleaning sheet
#'
#' @param data Name of sheet containing data.
#' @param cleaning Name of sheet containing cleaning code.
#' @param survey Name of sheet containing XLSForm survey.
#' @param choices Name of sheet containing XLSForm choices.
new_kobold <- function(data, cleaning, survey, choices) {
  object <- list(
    data = data,
    cleaning = cleaning,
    survey = survey,
    choices = choices
  )

  class(object) <- c("kobold", class(object))
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
