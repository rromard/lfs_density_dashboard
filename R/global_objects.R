## Relative path to union data folder
ud_path <- file.path(here() %>% dirname())

# Labour force survey selection vectors
lfs_default_vars = c(
  "rec_num","survyear","survmnth","ref_date","ref_qtr",
  "finalwt","finalwta","prov","cma","union","cowmain")
lfs_demo_vars = c("age_12","age_5","sex","efamtype","schooln","agyownk","educ","immig")
lfs_emp_vars = c("lfsstat", "mjh","ftptmain","permtemp","whypt","nse","prevtenure","tenure","estsize","firmsize","noc_10","noc_43","naics_21")
lfs_wages_hrs_vars = c("utothrs","hrlyearn","xtrahrs")
lfs_cols <- c(lfs_default_vars, lfs_demo_vars, lfs_emp_vars, lfs_wages_hrs_vars)

# Parameters for frequencies and proportions
lfs_params <- c("prov","cma","age_group","sex","educ_rc","schooln","fam_rc","agyownk","immig",
                "mjh","ftptmain","permtemp","nse","naics_21","noc_10","estsize","firmsize","tenure_rc") |> map(sym)