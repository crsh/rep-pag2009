#' Shuffle Covariate Values in a Data Frame
#'
#' This function shuffles the values of one or more covariates within a data frame based on a specified formula. 
#' The formula determines which variables are shuffled and how they are grouped during the shuffling process.
#' The shuffling is done in a way that maintains the relationship between the shuffled variables and other variables in the data frame.
#'
#' @param x A data.frame containing the variables to be shuffled.
#' @param formula A formula specifying one or more covariates to shuffle (left-hand side) and one or more grouping variables (right-hand side).
#'   The covariates will be shuffled within the levels of the grouping variables.
#' @return The data.frame `x` the specified covariates shuffled within the levels of the grouping variables.
#' @examples
#' data(pilot)
#' shuffled_pilot <- shuffle_variable(
#'   x = pilot,
#'   formula = acc_activity ~ condition * correct
#' )
#' @export

shuffle_variable <- function(x, formula) {
  vars <- parse_formula(formula)

  shuffled_covariate <- x |>
    dplyr::reframe(
      dplyr::across(
       !!vars$lhs
      , .fns = ~ if(length(unique(.x)) == 1) unique(.x) else .x
    )
    , .by = vars$rhs
  ) |>
    dplyr::mutate(
      dplyr::across(
        !!vars$lhs
        , .fns = ~ sample(.x)
        , .names = "{.col}_shuffled"
      )
    ) |>
    dplyr::select(-!!vars$lhs)

  x |>
    dplyr::select(-!!vars$lhs) |>
    dplyr::left_join(
      shuffled_covariate
      , by = vars$rhs
      , relationship = "many-to-many"
    ) |>
    dplyr::rename_with(
      \(.x) gsub("_shuffled", "", .x)
      , dplyr::all_of(paste0(vars$lhs, "_shuffled"))
    )
}
