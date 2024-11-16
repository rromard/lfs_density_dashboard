# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)

# Set target options:
tar_option_set(
  packages = c("tidyverse", "here", "zoo", "janitor", "data.table", "dtplyr"),
  format = "qs",
  error = NULL
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source("R/functions.R")
tar_source("R/global_objects.R")

# Pipeline
list(
  tar_target(lfs_raw_file,
    "../../../union_data/lfs_pipeline/data/lfs_base.qs",
    format = "file"
    ),
  tar_target(
    lfs_prep,
    lfs_prep_analysis(
      infile = lfs_raw_file, 
      years = 2006:2024,
      cols = lfs_cols
    ),
  ),
  tar_target(
    lfs_freq,
    lfs_freq_prop(lfs_prep, lfs_params)
  ),
  tar_target(
    lfs_freq_file, "data/lfs_freq.csv", format = "file"
  ),
  tar_target(
    lfs_freq_out, write_csv(lfs_freq, lfs_freq_file),
  )
)
