#' Get Recipes Based on Ingredients That You Have at Home
#'
#' This function suggests recipes that can be made
#' using your available ingredients at home
#' or simply ingredients that you want to use.
#'
#' The recipes are based on the Recipes.csv file.
#'
#' @param ingredients A character vector of ingredients you have or want to use.
#' @param file Path to the Recipes.csv file.
#' @return A character vector of recipe names and all of the relevant ingredients
#' @examples
#' get_recipes(c("chicken", "pepper", "flour"),
#'                      system.file("extdata", "Recipes.csv", package = "recipefinder"))
#' @export
find_recipes <- function(ingredients, file) {
  df <- read.csv(file, stringsAsFactors = FALSE)
  user_ingredients <- tolower(trimws(ingredients))

  parse_ingredients <- function(row_ingredients) {
    clean <- gsub("\\[|\\]|'", "", row_ingredients)
    items <- unlist(strsplit(clean, ",\\s*"))
    tolower(trimws(items))
  }

  match <- function(recipe_ingredients) {
    parsed <- parse_ingredients(recipe_ingredients)
    all(sapply(user_ingredients, function(user_ing) {
      any(grepl(user_ing, parsed, fixed = TRUE))
    }))
  }

  matched <- df[sapply(df$Ingredients, match), ]

  if (nrow(matched) == 0) {
    return("No recipes found.")
  }

  result <- lapply(seq_len(nrow(matched)), function(i) {
    ing <- parse_ingredients(matched$Ingredients[i])
    paste(matched$Title[i], "\n -", paste(ing, collapse = "\n - "))
  })

  return(result)
}
