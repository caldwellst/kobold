## Constructor for a basic class, which is essentially a list with data frames for data, cleaning sheet, survey, and choices.

new_kobold <- function(data, cleaning, survey, choices) {
   object <- list(data = data,
                  cleaning = cleaning,
                  survey = survey,
                  choices = choices)

   class(object) <- c("kobold", class(object))
   return(object)
}

## Defining generics for kobold class objects

## Load the actual data as a data frame if as.data.frame(kobold) is called

as.data.frame.kobold <- function(df) {
   as.data.frame(df$data)
}

## Load the actual data as a table if as_tibble(kobold) is called

as_tibble.kobold <- function(df) {
   as_tibble(df$data)
}

