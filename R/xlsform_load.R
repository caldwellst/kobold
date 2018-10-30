## Reading in the form

readxls_form <- function(file,
                         data = "data",
                         cleaning = "cleaning",
                         survey = "survey",
                         choices = "choices") {
   ## Select only the sheets of the file that we need
   worksheets <- readxl::excel_sheets(file)
   worksheets <- worksheets[worksheets %in% c(choices, survey, data, cleaning)]

   ## Loading the Excel sheets into a new class so we can make all names of the list objects the same
   object <- new_kobold(data = read_excel(file, sheet = data),
                        cleaning = read_excel(file, sheet = cleaning),
                        survey = read_excel(file, sheet = survey),
                        choices = read_excel(file, sheet = choices))

   ## Function for converting columns to the proper type within the data sheet of the kobold object
   convert_columns <- function(types, converter) {
      retype_names <- c(subset(object$survey, type %in% types)$name)
      retype_cols <- unique(retype_names[retype_names %in% names(object$data)])
      suppressWarnings(suppressMessages(
         object$data[retype_cols] <<- mutate_all(object$data[retype_cols], converter)
      ))
   }

   ## Converting the columns for each type
   convert_columns(c("decimal", "integer"), as.numeric)
   convert_columns(c("boolean"), as.logical)
   convert_columns(c("select one", "imei", "simserial", "deviceid", "phonenumber"), as.character)
   convert_columns(c("start", "end", "today"), as_datetime)

   ## And return the list!
   return(object)
}
