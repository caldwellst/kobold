## Constructor for a basic class, which is essentially a list with data frames for data, cleaning sheet, survey, and choices.

new_kobold <- function(data, cleaning, survey, choices) {
   object <- list(data = data,
                  cleaning = cleaning,
                  survey = survey,
                  choices = choices)

   class(object) <- c("kobold", class(object))
   return(object)
}
