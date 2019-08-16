#' Rename columns function
#'
#' @importFrom stringr str_detect
#' @importFrom rlang is_empty
#' @importFrom glue glue
#' @importFrom dplyr rename
#'
#' @noRd
rename_column <- function(sheet, rename, search, env) {
  names <- names(env$object[[sheet]])
  ind <- which(str_detect(names, search))
  if(is_empty(ind)) {
    warn(
      glue("Can't find {rename} column in {sheet}.")
    )
  } else {
    ind = ind[1]
    env$object[[sheet]] <- rename(env$object[[sheet]], !!rename := ind)
  }
}
