#' Generate a data analysis plan based on XLS Form survey
#'
#' @importFrom dplyr tibble
#' @importFrom stringr str_replace_all
generate_dap <- function(object) {
  # Initializing basic columns
  dap <- tibble(.rows = nrow(object$survey))
  dap$`Indicator / Variable` <- object$survey$name
  dap$`Questionnaire Question` <- object$survey$label
  dap$Instructions <- str_replace_all(object$survey$type,
                                      c("select_one(.*)" = "Select one",
                                        "select_multiple(.*)" = "Select multiple",
                                        "rank(.*)" = "Rank",
                                        "dateTime" = "Date and time",
                                        "xml-external" = "External data file"))
  dap$Instructions <- str_to_sentence(dap$Instructions)
}
