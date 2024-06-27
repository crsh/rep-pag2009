#' Equalize Means Across Conditions
#'
#' This function adjusts the means of specified variables across different conditions.
#' It calculates the mean of each variable for each combination of conditions, then subtracts the overall mean of these means from each,
#' effectively euqalizing the means of the variables across these groups.
#'
#' @param x A data.frame containing the to-be-equalized variables and condition variables.
#' @param formula A formula specifying the variables to be equalized and the condition variables.
#'
#' @return The data.frame `x` with the specified variables' means equalized across conditions.
#'
#' @examples
#' data(pilot)
#' 
#' equalized_data <- equalize_means(
#'   x = pilot,
#'   formula = cbind(acc_activity, dlpfc_activity) ~ condition * correct
#' )
#' 
#' @export

equalize_means <- function(x, formula) {
  vars <- parse_formula(formula)

  offsets <- x |>
  dplyr::summarize(
    dplyr::across(
       !!vars$lhs
      , .fns = ~ mean(.x)
    )
    , .by = vars$rhs
  ) |>
  dplyr::mutate_if(
    is.numeric
    , .funs = ~ .x - mean(.x)
  ) |>
  dplyr::rename_with(\(.x) paste0(.x, "_offset"), dplyr::all_of(vars$lhs))

  dplyr::left_join(
    x
    , offsets
    , by = vars$rhs
  ) |>
  dplyr::mutate(
    dplyr::across(
       !!vars$lhs
      , .fns = ~ .x - get(paste0(dplyr::cur_column(), "_offset"))
    )
  ) |>
  dplyr::select(-dplyr::ends_with("_offset"))
}


#' Equalize Average Decision Times
#'
#' This function fits a Bayesian shifted lognormal model to reaction times (or another dependent variable) across conditions.
#' It then calculates the log means for the dependent variable across unique combinations of the independent variables,
#' effectively equalizing the average decision times based on the model.
#'
#' @inheritParams equalize_means
#' @param formula A formula specifying the model to be fitted, i.e. the dependent variable and the condition variables.
#' @param ... Additional arguments passed to the `brms::brm` function.
#'
#' @return The data.frame `x` with the specified variable's average decision times equalized across conditions.
#'
#' @examples
#' \dontrun{
#' data(pilot)
#' 
#' equalized_rt <- equalize_logmeans(
#'   x = pilot
#'   , formula = rt ~ condition * correct
#'   , chains = 3
#'   , cores = 1
#'   , iter = 2000
#'   , warmup = 1000
#'   , verbose = FALSE
#'   , refresh = 0
#' )
#' }
#' @importFrom rlang .data
#' @export

equalize_logmeans <- function(x, formula, ...) {
  lnorm_mod <- brms::brm(
    formula = formula
    , data = x
    , family = brms::shifted_lognormal
    , ...
  )

  vars$lhs <- parse_formula(formula)

  offsets <- tidybayes::epred_draws(
    lnorm_mod
    , dpar = c(mu = "mu", ndt = "ndt")
    , newdata = x[, vars$lhs$rhs] |> unique()
    , re_formula = NULL
  ) |>
    dplyr::ungroup() |>
    dplyr::summarize(
      mu = stats::median(.data$mu)
      , ndt = stats::median(.data$ndt)
      , .by = dplyr::all_of(vars$lhs$rhs)
    ) |>
    dplyr::mutate(mu_offset = .data$mu - mean(.data$mu)) |>
    dplyr::select(-.data$mu)

  dplyr::left_join(
    x
    , offsets
    , by = vars$lhs$rhs
  ) |>
  dplyr::mutate(
    dplyr::across(
      !!vars$lhs$lhs
      , .fns = ~ exp(log(.x - ndt) - mu_offset) + ndt
    )
  ) |>
  dplyr::select(-.data$ndt, -.data$mu_offset)
}

parse_formula <- function(x) {
  lhs <- all.vars(x[-3])
  rhs  <- all.vars(x[-2])
  list(lhs = lhs, rhs = rhs)
}
