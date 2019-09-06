#' Function for identifying sheet, parent sheet, and group for each survey question
#'
#' @importFrom stringr str_detect str_trim
#' @importFrom glue glue_collapse
#'
#' @noRd
identify_groups <- function(env) {
  env$object$survey$sheet <- NA
  env$object$survey$parent <- NA
  env$object$survey$group <- NA

  begin_rgx <- "^.*(begin_group|begin group|begin repeat|begin_repeat)"
  end_rgx <- "^.*(end_group|end group|end repeat|end_repeat)"

  repeat_begin <- "^.*(begin_repeat|begin repeat)"
  repeat_end <- "^.*(end_repeat|end repeat)"

  sheet <- "data"
  parent <- c("")
  group <- c("")
  i <- 1

  while (i <= nrow(env$object$survey)) {
    type <- env$object$survey$type[i]

    if (str_detect(type, end_rgx)) {
      group <- group[-length(group)]
      if (str_detect(type, repeat_end)) {
        sheet <- parent[length(parent)]
        parent <- parent[-length(parent)]
      }
    }

    env$object$survey$sheet[i] <- sheet
    env$object$survey$parent[i] <- str_trim(glue_collapse(parent, sep = " "))
    env$object$survey$group[i] <- str_trim(glue_collapse(group, sep = " "))

    if (str_detect(type, begin_rgx)) {
      group <- append(group, env$object$survey$name[i])
      if (str_detect(type, repeat_begin)) {
        parent <- append(parent, sheet)
        sheet <- env$object$survey$name[i]
      }
    }
    i <- i + 1
  }
}

#' Function to identify list_name for select questions
#'
#' @importFrom stringr str_remove_all str_detect
#'
#' @noRd
identify_list_name <- function(env) {
  remove_rgx <- "^.*(select_multiple|select multiple| |select_one|select one)"
  select_rgx <- "^.*(select_multiple|select multiple|select_one|select one)"
  env$object$survey$list_name <- str_remove_all(env$object$survey$type,
                                                remove_rgx)
  env$object$survey$list_name[!str_detect(env$object$survey$type, select_rgx)] <- ""
}
