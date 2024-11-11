xfun::pkg_attach(
  c(
    'tidyverse',
    'targets',
    'canpumf',
    'janitor',
    'zoo',
    'here',
    'qs',
    'svrep',
    'data.table',
    'dtplyr',
    "glue",
    "rlang"
  )
)

lfs_df <- tar_read(lfs_prep)
lfs_df <- lazy_dt(lfs_df)
lfs_sub <- lfs_df |> filter(survyear%in%2023:2024)
tar_read(lfs_freq)
