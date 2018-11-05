## Function below comprise the cleaners of the system

## Change class of vectors based on the XLSForm survey design
#' @importFrom dplyr mutate filter if_else
#' @importFrom rlang sym
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
      if(!is.na(uuid)) {
         print("I'm cleaning by UUID!")
         kobold_file$data <<- mutate(kobold_file$data, !!name := ifelse(X_uuid == uuid, value,!! sym(name)))
      }

      else if(!is.na(relevant)){
         print("I'm cleaning by relevant")
         print(class(convert_relevant(relevant)))
         kobold_file$data <<- mutate(kobold_file$data, !!name := ifelse(!! convert_relevant(relevant), value,!! sym(name)))
      }

      else {
         warning("Changing all values in {name} to {value} since no UUID or relevant logic provided")
         kobold_file$data <<- mutate(kobold_file$data, !!name := value)
      }
   }

   remove_value <- function()

   ## General cleaning function to call the different components
   general_cleaner <- function(type, name, value, uuid, relevant) {
      if(type == "change_response") {
         change_response(name, value, uuid, relevant)
         print("I'm changing values!")
      }
      else if(type == "remove_survey") {
         remove_survey(uuid = uuid, relevant = relevant)
         print("I'm removing surveys!")
      }
      else {
         stop("Cleaning type is incorrect")
      }
   }

   mapply(general_cleaner, kobold_file$cleaning$type, kobold_file$cleaning$name, kobold_file$cleaning$value, kobold_file$cleaning$uuid, kobold_file$cleaning$relevant)
   return(kobold_file)
}
