## Function below comprise the cleaners of the system

#' @importFrom dplyr mutate filter
#' @importFrom rlang sym
#' @importFrom glue glue
#' @importFrom purrr pmap
#' @importFrom stringr str_remove str_trim str_replace
kobold_cleaner <- function(kobold_file) {

   ## Function to remove surveys based on either a UUID or relevant logic
   remove_survey <- function(uuid, relevant) {
      if(!is.na(uuid)) {
         kobold_file$data <<- filter(kobold_file$data, !(X_uuid == uuid))
      }
      else{
         stopifnot(!is.na(relevant))
         kobold_file$data <<- filter(kobold_file$data, !(!! convert_relevant(relevant)))
      }
   }

   ## Function to change value in a columns row(s) based on name of the cell, new value to place, and UUID or relevant logic.
   change_response <- function(name, value, uuid, relevant) {
      if(str_detect(c(filter(kobold_file$survey, name == name)$type), "^.*(select_multiple|select multiple)")) {
         warning(glue("Since {name} is a select_multiple question, change_response will change the entire response to {value}
                      \nTo only remove a single response option or add a single response option from a select_multiple response
                      \nuse remove_option and add_option respectively"))
      }
      if(!is.na(uuid)) {
         kobold_file$data <<- mutate(kobold_file$data, !!name := ifelse(X_uuid == uuid, value,!! sym(name)))
      }

      else if(!is.na(relevant)){
         kobold_file$data <<- mutate(kobold_file$data, !!name := ifelse(!! convert_relevant(relevant), value,!! sym(name)))
      }

      else {
         warning(glue("Changing all values in {name} to {value} since no UUID or relevant logic provided"))
         kobold_file$data <<- mutate(kobold_file$data, !!name := value)
      }
   }
   ## Removes singular value from a response. For non-select_multiple questions, just instead run
   ## change_response, since there is no need to deal with multiple response options.
   remove_option <- function(name, value, uuid, relevant) {
      if(!str_detect(c(filter(kobold_file$survey, name == name)$type), "^.*(select_multiple|select multiple)")) {
         change_response(name, value, uuid, relevant)
      }
      else if(!is.na(uuid)) {
         kobold_file$data <<- mutate(kobold_file$data, !!name := ifelse(X_uuid == uuid, select_mul_str_removal(!!sym(name), value), !!sym(name)))
      }

      else if(!is.na(relevant)) {
         kobold_file$data <<- mutate(kobold_file$data, !!name := ifelse(!! convert_relevant(relevant), select_mul_str_removal(!!sym(name), value), !!sym(name)))
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
         remove_option(name = name, value = value, uuid = uuid, relevant = relevant)
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

## Select multiple string removal function
select_mul_str_removal <- function(string, option) {
   option = glue("\\b{option}\\b")
   str_trim(str_replace(str_remove(string, option), "  ", " "))
}
