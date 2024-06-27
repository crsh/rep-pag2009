#' Pilot Study Data
#'
#' This dataset contains the results of one participant who piloted the replication study.
#'
#' @format A tibble with 140 rows and 8 variables:
#' \describe{
#'   \item{participant_id}{Unique identifier of the participant.}
#'   \item{dishonesty_score}{Dishonesty score of the participant calcluated as the relative frequency of claimed wins in the opportunity-to-cheat condition.}
#'   \item{trail_number}{Running index of trials within the experiment.}
#'   \item{condition}{Factor indicating the experimental condition, with `op` indicating the opportunity to cheat and `noop` indicating no opportunity.}
#'   \item{correct}{Indicates whether the participant claimed a correct prediction (`win`) or not (`loss`).}
#'   \item{rt}{Numeric. Response time in milliseconds.}
#'   \item{acc_activity}{Numeric. fMRI BOLD signal measured in the anterior cingulate cortex.}
#'   \item{dlpfc_activity}{Numeric. fMRI BOLD signal measured in the dorso-lateral preforntal cortex.}
#' }
#' @usage
#' data(pilot)
#' @examples
#' data(pilot)
#' summary(pilot)
#' boxplot(rt ~ condition * correct, data = pilot)

"pilot"
