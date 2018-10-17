## Functions below convert skip logic (relevant) coding from XLS Form into R strings.
## Once the entire string is converted, it's passed through rlang::parse_expr() into
## a filter and evaluated to, mimic the working of XLS Form coding.

## Convert the selected function from XLS Form into a string filter statement for dplyr
## selected(${variable_a}, 'response_a')

convert_selected <- function(string) {
   var <- var_extract(string)
   answer <- str_match(string, "(?:\'|\")(.*)(?:\'|\")")[2]
   paste0(var, " %in% '", answer, "'")
}

## Extract variable names from the XLS Form Standard
## ${var_name}

var_extract <- function(string) {
   str_match(string, "\\$\\{(.*?)\\}")[,2]
}

## Convert count-selected(), which tells you how many times a select_multiple question was selected
## Use number of spaces in the column to count this
## count-selected(${variable_a}) LOGICAL #
## E.g. count-selected(${coping_mechanisms}) > 3

convert_countsel <- function(string) {
   var <- var_extract(string)
   paste("str_count(", var, ", ' ')", sep = "")
}

## Final converter, which will convert the relevant sting into R logic, using functions above

convert_relevant <- function(string) {
   string <- str_replace_all(string, "count-selected\\((.*?)\\)", convert_countsel)
   string <- str_replace_all(string, "selected\\((.*?)\\)", convert_selected)
   string <- str_replace_all(string, c("and" = "&", "or" = "|", "not" = "!"))
   string <- str_replace_all(string, "\\$\\{(.*?)\\}", var_extract)
   return(string)
}
