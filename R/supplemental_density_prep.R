# library(tidyverse)
# library(here)
# library(janitor)
# library(qs)
# library(haven)
# 
# 
# # Overall union density 1921-2024 -----------------------------------------
# prep_df <- qread(here('_targets', 'objects', 'lfs_prep'))
# # Source https://www150.statcan.gc.ca/n1/daily-quotidien/170908/cg-a003-png-eng.htm
# hist_df <- read.csv(here('data', 'density_historical.csv'))
# 
# lfs_dens <- prep_df |>
#   filter(union_bin != "Not applicable", 
#          str_detect(lfsstat, "Employed"),
#          naics_21 != "Agriculture") |> 
#   count(survyear, union_bin, wt = finalwta) |> 
#   group_by(survyear) |> 
#   mutate(p = (n / sum(n))*100) |> 
#   filter(union_bin == "Union member") |> 
#   select(-union_bin, -n)
# 
# dens_hist <- hist_df |> 
#   as_tibble() |> 
#   rename(survyear = 1, p = 2) |> 
#   bind_rows(lfs_dens |> filter(survyear >= 2017), .id = "source") |> 
#   mutate(
#     era = case_when(
#       survyear < 1944 ~ "Grey-zone",
#       between(survyear, 1944, 1965) ~ "Private sector only",
#       .default = "All workers"
#     )
#   )
# 
# dens_hist |> 
#   select(-source) |> 
#   pivot_wider(names_from = era, values_from = p) |> 
#   clipr::write_clip()
# 
# write_csv(dens_hist, here('data', 'density_1921_2024.csv'))
# 
# # Density by sector -------------------------------------------------------
# lfs_sec_dens <- prep_df |>
#   filter(union_bin != "Not applicable",
#          cowmain_rc != "Self-employed",
#          str_detect(lfsstat, "Employed"),
#          naics_21 != "Agriculture") |> 
#   count(survyear, cowmain = cowmain_rc, union_bin, wt = finalwta) |> 
#   group_by(survyear, cowmain) |> 
#   mutate(p = (n / sum(n))*100) |> 
#   filter(union_bin == "Union member") |> 
#   select(-union_bin)
# 
# # Take a long time to load the SPSS file
# lfs_long_raw <- read_sav("D://ds_projects//data//statcan//lfs//LFS 1987- Present (2020-12).sav")
# 
# lfs_long_df <- lfs_long_raw |>
#   clean_names() |> 
#   select(
#     rec_num,
#     survyear,
#     survmnth,
#     lfsstat16,
#     fweight,
#     cowmain,
#     union,
#     starts_with("naics")
#   ) |> 
#   filter(survyear %in% 1997:2005,
#          lfsstat16 %in% c(1,2),
#          cowmain %in% c(1,2))
# 
# lfs_long_dens <- lfs_long_df |> 
#   filter(naics_43 != 1) |> 
#   mutate(fweighta = fweight/12,
#          across(cowmain:union, as_factor),
#          union_bin = fct_collapse(union, 
#                                   `No union` = c('Not member or covered', 'Not member, covered by collective aggreement'))) |> 
#   count(survyear, cowmain, union_bin, wt = fweighta) |> 
#   group_by(survyear, cowmain) |> 
#   mutate(p = n / sum(n) * 100) 
# 
# lfs_long_dens |> 
#   filter(union_bin == "Union member") |> 
#   select(-union_bin) |> 
#   bind_rows(lfs_sec_dens) |> 
#   mutate(cowmain = as.character(cowmain),
#          cowmain = ifelse(
#            str_detect(cowmain, "Public"), "Public sector", "Private sector"
#          )) |> 
#   select(-n) |> 
#   pivot_wider(names_from = cowmain, values_from = p) |> 
#   clipr::write_clip()
