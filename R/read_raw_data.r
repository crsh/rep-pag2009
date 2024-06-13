#' Read Raw Data
#'
#' This function reads all .xlsx files from a specified directory and combine them into a single data frame.
#' The column names of the data frame are cleaned using the janitor::make_clean_names function.
#'
#' @param x A character string specifying the directory where the .xlsx files are located. Default is "data-raw/exp/".
#' 
#' @return A data frame containing the combined data from all .xlsx files in the specified directory.
#' 
#' @examples
#' \dontrun{
#' read_raw_data("data-raw/exp/")
#' }
#' 
#' @export

read_raw_data <- function(x = "data-raw/exp/") {
  data_files <- list.files(
    x
    ,  full.names = TRUE
    , pattern = ".xlsx"
  )

  dat <- lapply(
    data_files
    , readxl::read_xlsx
    , .name_repair = ~ janitor::make_clean_names(.x)
  ) |>
    do.call("rbind", args = _) |>
    dplyr::rename(rt = "behavioral_response")

  dat
}
