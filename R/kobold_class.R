## Constructor for a basic class, which is essentially a list with data frames for data, cleaning sheet, survey, and choices.
#' @title Create a kobold list from an XLS Form
#' @description This is a constructor of a kobold list using 4 components of an XLS Form survey, data outputs, and the kobold cleaning sheet.
#'
#' @param data Data frame containing exported data
#' @param cleaning Data frame containing cleaning sheet
#' @param survey Data frame containing XLS Form survey
#' @param choices Data frame containing XLS Form choices
new_kobold <- function(data, cleaning, survey, choices) {
   object <- list(data = data,
                  cleaning = cleaning,
                  survey = survey,
                  choices = choices)

   class(object) <- c("kobold", class(object))
   return(object)
}

## Defining generics for kobold class objects

## data.frame generic for kobold
#' @export
data.frame.kobold <- function(df) {
   data.frame(df$data)
}

## as_tibble generic for kobold
#' @importFrom tibble as_tibble
#' @export
as_tibble.kobold <- function(df) {
   as_tibble(df$data)
}


