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
#' @param max_minutes Maximum time (in minutes) to filter recipes. Default is Inf (no filter).
#' @return A character vector of recipe names, all of the relevant ingredients and directions.
#' @examples
#' find_recipes(c("chicken", "pepper", "flour"),
#'                      system.file("extdata", "Recipes.csv", package = "recipefinder"), 30)
#' @export
find_recipes <- function(ingredients, file, max_minutes = Inf) {
  df <- read.csv(file, stringsAsFactors = FALSE)

  names(df) <- trimws(names(df))

  # Remove duplicate recipes
  df <- df[!duplicated(df$recipe_name), ]

  user_ingredients <- tolower(trimws(ingredients))

  # Parse total_time into numeric minutes
  parse_total_time <- function(time_str) {
    time_str <- tolower(time_str)
    hrs <- ifelse(grepl("hr", time_str), as.numeric(sub("^(\\d+).*", "\\1", time_str)), 0)
    mins <- ifelse(grepl("min", time_str), as.numeric(sub(".*?(\\d+)\\s*min.*", "\\1", time_str)), 0)
    hrs[is.na(hrs)] <- 0
    mins[is.na(mins)] <- 0
    hrs * 60 + mins
  }

  df$total_minutes <- sapply(df$total_time, parse_total_time)
  df <- df[df$total_minutes <= max_minutes, ]

  user_ingredients <- tolower(trimws(ingredients))

  parse_ingredients <- function(row_ingredients) {
    items <- unlist(strsplit(row_ingredients, ",\\s*"))
    tolower(trimws(items))
  }

  matches <- sapply(df$ingredients, function(row) {
    parsed <- parse_ingredients(row)
    all(sapply(user_ingredients, function(user_ing) {
      any(grepl(user_ing, parsed, fixed = TRUE))
    }))
  })

  matched <- df[matches, ]

  if (nrow(matched) == 0) {
    return("No recipes found.")
  }

  result <- lapply(seq_len(nrow(matched)), function(i) {
    ing <- parse_ingredients(matched$ingredients[i])
    paste0(
      "🍽️ ", matched$recipe_name[i], "\n\n",
      "⏱️ Total time: ", matched$total_time[i], "\n\n",
      "🧂 Ingredients:\n- ", paste(ing, collapse = "\n- "), "\n\n",
      "📖 Directions:\n", matched$directions[i]
    )
  })

  return(unique(result))
}

