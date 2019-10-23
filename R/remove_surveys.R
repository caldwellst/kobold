#' Remove entries from loop data sheets
#'
#' @importFrom dplyr filter
#' @importFrom rlang !!
#'
#' @noRd
remove_entry <- function(sheet, var_uuid, var_index, relevant, env) {
  filter_expr <- filter_expr_generator(var_index, var_uuid, relevant)
  print(var_index)
  var_index <- filter(env$object[[sheet]], !!filter_expr)$index
  env$object[[sheet]] <- filter(env$object[[sheet]], !(!!filter_expr))
  child_entry_remover(sheet, var_index, env)
}

#' Remove entries from nested loop sheets
#'
#' @importFrom dplyr filter
#' @importFrom purrr pmap
#'
#' @noRd
child_entry_remover <- function(sheet, var_index, env) {
  children <- filter(env$object$data_sheets, parent == sheet)$sheets
  sheet_num <- length(children)
  if (sheet_num > 0) {
    for (i in 1:sheet_num) {
      indices <- filter(env$object[[children[i]]], parent_index %in% var_index)$index
      env$object[[children[i]]] <- filter(env$object[[children[i]]], !(parent_index %in% var_index))
      child_entry_remover(children[i], indices, env)
    }
  }
}
