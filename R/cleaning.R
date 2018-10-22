## Function below comprise the cleaners of the system

## Simple remove survey. They specify survey to remove, we remove

remove_survey <- function(df, filter) {
   df <- filter(df, eval(filter))

}
