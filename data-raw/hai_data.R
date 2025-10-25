## code to prepare `hai_data` dataset goes here

library(BHAI)
library(tidyverse)
library(methods)

devtools::load_all()

german_pps_conv_df <- healthburdenr::flatten_pps(BHAI::german_pps_conv)
eu_pps_df <- healthburdenr::flatten_pps(BHAI::eu_pps)

usethis::use_data(german_pps_conv_df, eu_pps_df, overwrite = TRUE)
