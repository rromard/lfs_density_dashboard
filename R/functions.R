# Script to define functions for use in target pipeline

# Libraries ---------------------------------------------------------------

xfun::pkg_attach(c('tidyverse', 'canpumf', 'janitor', 'zoo', 'here', 'qs', 
                   'data.table', 'dtplyr'))

# Functions ---------------------------------------------------------------

# Prep and import
lfs_variable_recode <- function(df) { 
  
  # Key with n months per survyear for adjusting the weights to an annual basis
  yr_key <- df |> 
    distinct(survyear, survmnth) |> count(survyear, name = "n_months")
  
  df |>
    left_join(yr_key, by = 'survyear') |>
    lazy_dt() |> 
    mutate(
      # Annualized weights (divide by number of months of data)
      finalwta = finalwt / n_months,
      
      # Labour force status - collapse employed into one group
      lfsstat_rc = fct_collapse(
        lfsstat,
        "Employed" = c("Employed, at work", "Employed, absent from work")
      ),
      
      # Binary union measure, member or not
      union_bin = fct_collapse(
        union,
        "Not member" = c(
          "Non-unionized",
          "Not a member but covered by a union contract or collective agreement"
        )
      ),
      
      # Binary union measure, member or covered by CA
      union_cov = fct_collapse(
        union,
        "Member/Covered" = c(
          "Union member",
          "Not a member but covered by a union contract or collective agreement"
        )
      ),
      
      # Class of worker
      cowmain_rc = fct_collapse(
        cowmain,
        "Self-employed" = c(
          "Self-employed unincorporated, no paid help",
          "Self-employed incorporated, with paid help",
          "Self-employed incorporated, no paid help",
          "Self-employed unincorporated, with paid help",
          "Unpaid family worker"
        )
      ),
      
      # Non-standard employment (part-time, temporary, multi-job holders, precarious self-employed)
      # Typology source: https://www.erudit.org/en/journals/ri/2022-v77-n1-ri06959/1088554ar/
      nse = case_when(
        # Unemployed and not in labour force
        str_detect(lfsstat, "Unemployed") ~ "Unemployed",
        lfsstat == "Not in labour force" ~ "Not in labour force",
        # Multi-job holders
        str_detect(mjh, "Multiple") ~ "Non-standard employment",
        # Precarious self-employment (FT no help, or PT even with help)
        cowmain == "Self-employed unincorporated, no paid help" ~ "Non-standard employment",
        # All part-time (including self-employment) and temporary employment
        ftptmain == "Part-time" ~ "Non-standard employment",
        permtemp != "Permanent" ~ "Non-standard employment",
        # Standard employment - full-time, permanent, not self-employed
        ftptmain == "Full-time" &
          str_detect(mjh, "Single") &
          permtemp == "Permanent" &
          cowmain %in%
          c(
            "Public sector employees",
            "Private sector employees",
            "Public employee",
            "Private employee"
          ) ~ "Standard employment",
        # Non-precarious self-employment (full-time with employees)
        cowmain %in% c(
          "Self-employed incorporated, with paid help",
          "Self-employed unincorporated, with paid help"
        ) & ftptmain == "Full-time" ~ "Standard employment",
        .default = NA_character_
      ),
      
      # Age groups
      age_group = case_when(
        str_detect(age_12, "15|20") ~ "15 to 24",
        str_detect(age_12, "25|30|35|40|45|50") ~ "25 to 54",
        str_detect(age_12, "55|60") ~ "55 to 64",
        str_detect(age_12, "65|70") ~ "65+"
      ),
      age_group = as_factor(age_group),
      age_group = fct_relevel(age_group,"15 to 24","25 to 54","55 to 64","65+"),
      
      # Education
      educ_rc = case_when(
        educ %in% c("0 to 8 years", "Some high school") ~ "Under secondary",
        educ == "High school graduate" ~ "Secondary/Some PSE",
        educ == "Some postsecondary" ~ "Secondary/Some PSE",
        educ == "Bachelor's degree" ~ "Undergraduate",
        educ == "Above bachelor's degree" ~ "Graduate",
        educ == "Postsecondary certificate or diploma" ~ "College",
        .default = as.character(educ)
      ),
      educ_rc = as_factor(educ_rc),
      educ_rc = fct_relevel(educ_rc,
                            "Under secondary",
                            "Secondary/Some PSE",
                            "College",
                            "Undergraduate",
                            "Graduate"
      ),
      
      # # Family type, collapse to more usable levels
      fam_rc = case_when(
        str_detect(efamtype, "Dual-earner") ~ "Dual-earner",
        str_detect(efamtype, "Lone-parent") ~ "Lone-parent",
        str_detect(efamtype, "Non-earner") ~ "Non-earner",
        str_detect(efamtype, "Dual-earner") ~ "Dual-earner",
        str_detect(efamtype, "Single-earner") ~ "Single-earner",
        efamtype == "Person not in an economic family" ~ "Not in econ family",
        .default = as.character(efamtype)
      ),
      ## Set factor levels
      fam_rc = factor(
        fam_rc,
        levels = c(
          "Non-earner",
          "Not in econ family",
          "Single-earner",
          "Lone-parent",
          "Dual-earner",
          "Other families"
        )
      ),
      
      # Binary measure, has children or not
      has_children = fct_collapse(
        agyownk,
        "has_children" = c(
          "Youngest child less than 6 years",
          "Youngest child 6 to 12 years",
          "Youngest child 13 to 17 years",
          "Youngest child 18 to 24 years"
        ),
        "no_children" = "Not applicable"
      ),
      # Job tenure, categories from https://www150.statcan.gc.ca/n1/pub/14-28-0001/2024001/article/00007-eng.htm
      tenure_rc = cut(
        tenure,
        c(1, 12, 60, 120, Inf),
        right = FALSE,
        include.lowest = TRUE
      )
    ) |> as_tibble()
}

# Import, select columns, run variable recode function
lfs_prep_analysis <- function(infile, cols, years, outfile = "data/lfs_prep.qs") {

qread(infile) |> 
  lazy_dt() |> 
  filter(survyear %in% years) |> 
  select(any_of(lfs_default_vars),
         any_of(cols)) |> 
  as_tibble() |> 
  lfs_variable_recode() |> 
  as_tibble()

}

# Frequencies, proportions, summary statistics
lfs_summarise <- function(.df, ...) {
  .df %>%
    count(..., wt = finalwt)
}

lfs_prop <- function(.df, ...) {
  .df %>%
    group_by(...) |> 
    mutate(
      p = n / sum(n, na.rm = TRUE)
    )
}

lfs_valid_p <- function(.df, x, level, ...) {
  
  .df |> 
    mutate( 
      # Calculate valid percent, excluding missing factor levels using helper column
      n_v = ifelse({{x}}==level | cowmain_rc == "Self-employed", NA, n),
      n_v = ifelse(
        varlevel == "Not applicable" & variable %in% c("naics_21", "noc_10", "ftptmain", "permtemp"),
        NA, n_v
      )
    ) |> 
    group_by(...) |> 
    mutate(
      p_v = n_v / sum(n_v, na.rm = TRUE)
    ) |> ungroup()
}

lfs_freq_prop <- function(df, .params) { 
  map(.params, \(x)
    df |> 
        lfs_summarise(ref_date, "varlevel" := get(x), cowmain_rc, union_bin) |> 
        lfs_prop(ref_date, varlevel, cowmain_rc)
  ) |> 
    set_names(nm = lfs_params) |> 
    map(as_tibble) |> 
    bind_rows(.id = "variable") |> 
    lfs_valid_p(union_bin, level = "Not applicable", ref_date, varlevel, cowmain_rc)
}


