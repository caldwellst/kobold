## Function below comprise the cleaners of the system

#' @importFrom dplyr mutate filter
#' @importFrom rlang sym
#' @importFrom glue glue
#' @importFrom purrr pmap map detect_index
#' @importFrom stringr str_remove str_trim str_replace
#' @importFrom R.utils insert
kobold_cleaner <- function(kobold_file) {

   ## Function to remove surveys based on either a UUID or relevant logic
   remove_survey <- function(uuid, relevant) {
      if(uuid != "") {
         kobold_file$data <<- filter(kobold_file$data, !(X_uuid == uuid))
      }
      else{
         stopifnot(relevant != "")
         kobold_file$data <<- filter(kobold_file$data, !(!! convert_xls_code(relevant)))
      }
   }

   ## Function to change value in a columns row(s) based on name of the cell, new value to place, and UUID or relevant logic.
   change_response <- function(name, value, uuid, relevant) {
      if(uuid != "") {
         kobold_file$data <<- mutate(kobold_file$data, !!name := ifelse(X_uuid == uuid, value,!! sym(name)))
      }

      else if(relevant != ""){
         kobold_file$data <<- mutate(kobold_file$data, !!name := ifelse(!! convert_xls_code(relevant), value,!! sym(name)))
      }

      else {
         warning(glue("Changing all values in {name} to {value} since no UUID or relevant logic provided"))
         kobold_file$data <<- mutate(kobold_file$data, !!name := value)
      }
   }

   ## Removes singular value from a response. For non-select_multiple questions, just instead run
   ## change_response, since there is no need to deal with multiple response options.
   remove_option <- function(q_name, value, uuid, relevant) {
      # if(str_detect(c(filter(kobold_file$survey, name == name)$type), "^.*(select_multiple|select multiple)")) {
      #    warning(glue("Since {name} is a select_multiple question, change_response will change the entire response to {value}
      #                 \nTo only remove a single response option or add a single response option from a select_multiple response
      #                 \nuse remove_option and add_option respectively"))
      # }

      if(!str_detect(c(filter(kobold_file$survey, name == q_name)$type), "^.*(select_multiple|select multiple)")) {
         change_response(q_name, value, uuid, relevant)
         warning(glue("remove_option is removing the entire response {value} since {q_name} is a select_one question"))
      }

      else { ## Here we get the name of the select_multiple binary column to change the value for
         binary_name <- unique(names(kobold_file$data %>%
                                   select(matches(paste0("(\\b", q_name, ")(.)(", value, "\\b)")),
                                          -one_of(c(kobold_file$survey$name)))))
         print(binary_name)
      }

      if(uuid != "") { ## making the changes if based on UUID
         kobold_file$data <<- mutate(kobold_file$data,
                                     !!q_name := ifelse(X_uuid == uuid, select_mul_str_removal(value, !!sym(q_name)), !!sym(q_name)),
                                     !!binary_name := ifelse(X_uuid == uuid, FALSE, !!sym(binary_name)))
      }

      else if(relevant != "") { ## making the changes if based on relevant logic
         kobold_file$data <<- mutate(kobold_file$data,
                                     !!q_name := ifelse(!! convert_xls_code(relevant), select_mul_str_removal(value, !!sym(q_name)), !!sym(q_name)),
                                     !!binary_name := ifelse(!! convert_xls_code(relevant), FALSE, !!sym(binary_name)))
      }
   }

   add_option <- function(q_name, value, uuid, relevant) {
      if(!str_detect(c(filter(kobold_file$survey, name == q_name)$type), "^.*(select_multiple|select multiple)")) {
         stop(glue("add_option failed to add {value} to {q_name} since it is not a select_multiple question"))
      }

      else { ## Generating the name of the binary column and also the order in which it should be inserted
         binary_name <- unique(names(kobold_file$data %>%
                                     select(matches(paste0("(\\b", q_name, ")(.)(", value, "\\b)")))))
         l_name <- str_trim(str_extract(filter(kobold_file$survey, name == q_name)$type, "(?<=select_multiple).*$"))
         choices <- filter(kobold_file$choices, list_name == l_name)$name
         choice_loc <- match(value, choices)
         }

      if(uuid != "") {
         kobold_file$data <<- mutate(kobold_file$data,
                                     !!q_name := ifelse(X_uuid == uuid, select_mul_str_adder(value, !!sym(q_name), choices, choice_loc), !!sym(q_name)),
                                     !!binary_name := ifelse(X_uuid == uuid, TRUE, !!sym(binary_name)))
      }

      else if(relevant != "") {
         kobold_file$data <<- mutate(kobold_file$data,
                                     !!q_name := ifelse(!! convert_xls_code(relevant), select_mul_str_adder(value, !!sym(q_name), choices, choice_loc), !!sym(q_name)),
                                     !!binary_name := ifelse(!! convert_xls_code(relevant), TRUE, !!sym(binary_name)))
      }
   }

   ## General cleaning function to call the different components
   general_cleaner <- function(type, name, value, uuid, relevant) {
      if(type == "change_response") {
         change_response(name, value, uuid, relevant)
      }
      else if(type == "remove_survey") {
         remove_survey(uuid = uuid, relevant = relevant)
      }
      else if(type == "remove_option") {
         remove_option(q_name = name, value = value, uuid = uuid, relevant = relevant)
      }

      else if(type == "add_option") {
         add_option(q_name = name, value = value, uuid = uuid, relevant = relevant)
      }

      else {
         stop(glue("Cleaning type {type} is incorrect"))
      }
   }

   pmap(list(kobold_file$cleaning$type,
             kobold_file$cleaning$name,
             kobold_file$cleaning$value,
             kobold_file$cleaning$uuid,
             kobold_file$cleaning$relevant),
        general_cleaner)
   return(kobold_file)
}

## Removes select_multiple value for cleaning
#' @importFrom glue glue
#' @importFrom stringr str_trim str_replace str_remove
select_mul_str_removal <- function(value, column) {
   option = glue("\\b{value}\\b")
   str_trim(str_replace(str_remove(option, column), "  ", " "))
}

## Adds select_multiple value for cleaning\
#' @importFrom glue glue
#' @importFrom stringr str_split
#' @importFrom purrr map
select_mul_str_adder <- function(value, column, choices, choice_loc) {
   split <- str_split(column, pattern = " ")
   indices <- map(split, match, choices)
   insert_index <- map(indices, function(x) which(order(c(choice_loc, x)) == 1))
   exist_test <- map_lgl(indices, function(x) is.na(match(choice_loc, x)))
   split <- ifelse(exist_test, map(split, function(x) append(x, value, after = choice_loc - 1)), split)
   map_chr(split, str_c, collapse = " ")
}
