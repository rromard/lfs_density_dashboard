# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)

tar_pkg <- c(
  "tidyverse",
  "quarto",
  "targets",
  "here",
  "scales",
  "qs",
  "zoo",
  "data.table",
  "dtplyr",
  "patchwork",
  "shiny",
  "shinyjs",
  "rlang",
  "ggrepel",
  "ggtext",
  "ggfittext",
  "kableExtra",
  "glue"
)

# Set target options:
tar_option_set(
  packages = tar_pkg,
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
    lfs_freq_prep,
    dash_base_data_prep(lfs_freq)
  ),
  tar_target(
    density_df,
    density_data_prep(lfs_freq_prep)
  ),
  tar_target(
    dist_df,
    dist_data_prep(lfs_freq_prep)
  ),
  tar_quarto(
    lfs_dashboard, path = "index.qmd"
  )
)
