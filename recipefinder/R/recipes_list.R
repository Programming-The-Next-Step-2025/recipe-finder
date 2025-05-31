#' List All Recipes
#'
#' Returns a character vector of all recipe titles available in the package's recipe database.
#'
#' @param file Path to the Recipes.csv file. Default is the internal copy in extdata.
#'
#' @return A character vector of all the recipe titles.
#' @examples
#' recipes_list()
#' @export
recipes_list <- function(
<<<<<<< HEAD
    file = system.file("extdata", "Recipes.csv", package = "recipefinder")
=======
    file# = system.file("extdata", "Recipes.csv", package = "recipefinder")
>>>>>>> week-3
) {
  df <- read.csv(file, stringsAsFactors = FALSE)
  return(df$Title)
}
