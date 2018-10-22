readxls_form <- function(file,
                         choices = "choices",
                         survey = "survey",
                         data = "data",
                         cleaning = "cleaning") {
   ## Select only the sheets of the file that we need
   worksheets <- readxl::excel_sheets(file)
   worksheets <- worksheets[worksheets %in% c(choices, survey, data, cleaning)]

   ## Loading all data into a list
   file_list <- lapply(worksheets, function(x) {
      df <- read_excel(file, sheet = x)
   })

   name_frame <- data.frame(matrix(c(choices, survey, data, cleaning, "choices", "survey", "data", "cleaning"),
                                   nrow = 4,
                                   ncol = 2),
                            stringsAsFactors = F)

   worksheets <- sapply(worksheets, function(x) {
      name_frame[name_frame[,1] == x, 2]
   })
   names(file_list) <- worksheets

   return(file_list)
}
