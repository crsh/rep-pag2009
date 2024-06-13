
# Process pilot data ----
source("R/read_raw_data.r")

pilot <- read_raw_data(x = "data-raw/pilot/")

usethis::use_data(pilot, overwrite = TRUE)
