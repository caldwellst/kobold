#' Clean XLSForm survey data
#'
#' @importFrom rlang current_env abort
#' @importFrom purrr pmap
#'
#' @export
kobold_cleaner <- function(object) {

  # Getting current environment
  env <- current_env()

  # General cleaning function ------------------------------------------------------------------------------

  general_cleaner <- function(type, name, value, sheet, uuid, index, relevant, cleaning_row, env) {
    print(cleaning_row)
    if (type == "change_response") {
      change_response(
        name,
        value,
        uuid,
        index,
        relevant,
        env
      )
    } else if (type == "remove_survey") {
      remove_entry(
        "data",
        uuid,
        index,
        relevant,
        env
      )
    } else if (type == "remove_loop_entry") {
      remove_entry(
        sheet,
        uuid,
        index,
        relevant,
        env
      )
    } else if (type == "remove_option") {
      remove_option(
        name,
        value,
        uuid,
        index,
        relevant,
        env
      )
    } else if (type == "add_option") {
      add_option(
        name,
        value,
        uuid,
        index,
        relevant,
        env
      )
    } else {
      abort(glue("Cleaning type {type} is incorrect"))
    }
  }

  pmap(
    list(
      object$cleaning$type,
      object$cleaning$name,
      object$cleaning$value,
      object$cleaning$sheet,
      object$cleaning$uuid,
      object$cleaning$index,
      object$cleaning$relevant,
      1:nrow(object$cleaning)
    ),
    general_cleaner, env
  )

  # Updating the relevant logic for the data
  object <- relevant_updater(object)

  # Converting the columns for each type
  class_converter(env)

  return(object)
}
