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
      
      ## Family type, collapse to more usable levels
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
      
      # Recode not applicable to no children
      agyownk = fct_recode(agyownk, "No children" = "Not applicable"),
      
      # Job tenure, categories from https://www150.statcan.gc.ca/n1/pub/14-28-0001/2024001/article/00007-eng.htm
      tenure_rc = cut(
        tenure,
        c(1, 12, 60, 120, Inf),
        right = FALSE,
        include.lowest = TRUE
      ),
      
      # Set some factor levels to NA
      across(c("naics_21", "noc_10", "ftptmain", "permtemp","firmsize", "estsize", "schooln"),
             \(x) fct_na_level_to_value(x, "Not applicable"))
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
## Counts
lfs_summarise <- function(.df, ...) {
  .df %>%
    count(..., wt = finalwt)
}

## Proportions
lfs_prop <- function(.df, ...) {
  .df %>%
    group_by(...) |> 
    mutate(
      p = n / sum(n, na.rm = TRUE)
    )
}

lfs_freq_prop <- function(df, .params) {
  map(
    .params,
    \(x)
    df |>
      filter(
        lfsstat %in% c("Employed, at work", "Employed, absent from work"),
        !cowmain_rc %in% c("Not applicable","Self-employed"),
        union_bin != "Not applicable") |> 
    lfs_summarise(survyear, survmnth, "varlevel" := get(x), cowmain_rc, union_bin) |>
    lfs_prop(survyear, survmnth, varlevel, cowmain_rc)
  ) |>
    set_names(nm = lfs_params) |>
    map(as_tibble) |>
    bind_rows(.id = "variable")
}

# Dashboard data prep

dash_base_data_prep <- function(df) {

df |>
  filter(
    varlevel != "Non-earner",
    varlevel != "Agriculture"
  ) |>
  select(survyear, survmnth, variable, varlevel, cowmain_rc, union_bin, n, p) |>
  mutate(
    var_orig = variable,
    var_orig = case_when(
      var_orig == "nse" ~ "Standard/Non-standard employment",
      var_orig == "ftptmain" ~ "Full-time/Part-time", 
      var_orig == "permtemp" ~ "Permament/Temporary", 
      var_orig == "mjh" ~ "Single/Multi-job holder",
      .default = as.character(var_orig)
    ),
    # Variable names to final labels
    variable = case_when(
      variable %in% c("nse", "ftptmain", "permtemp", "mjh") ~ "Employment status",
      variable == "prov" ~ "Province",
      variable == "cma" ~ "CMA",
      variable == "age_group" ~ "Age group",
      variable == "sex" ~ "Gender",
      variable == "educ_rc" ~ "Education level",
      variable == "schooln" ~ "Student status",
      variable == "fam_rc" ~ "Family type",
      variable == "agyownk" ~ "Youngest child",
      variable == "immig" ~ "Immigration",
      variable == "naics_21" ~ "Industry",
      variable == "noc_10" ~ "Occupation",
      variable == "tenure_rc" ~ "Job tenure",
      variable == "estsize" ~ "Establishment size",
      variable == "firmsize" ~ "Firm size"
    ),
    union_bin = fct_rev(union_bin),
    cowmain_rc = fct_relabel(cowmain_rc, \(x) str_remove(x, " employees")),
    cowmain_rc = fct_rev(cowmain_rc),
    
    # Setting some industry level variables to missing manually
    p = case_when(
      variable == "Industry" &
        cowmain_rc == "Public sector" &
        varlevel %in% c(
          "Agriculture",
          "Accommodation and food services",
          "Fishing, hunting and trapping",
          "Forestry and logging and support activities for forestry",
          "Manufacturing - durable goods",
          "Manufacturing - non-durable goods",
          "Mining, quarrying, and oil and gas extraction",
          "Other services (except public administration)",
          "Wholesale trade"
        ) ~ NA,
      variable == "Industry" &
        cowmain_rc == "Private sector" &
        varlevel == "Public administration" ~ NA,
      .default = p
    ),
    
    # Setting some factor levels to NA
    
    # Recoding some factor levels to be more compact
    varlevel = case_when(
      
      # Province
      varlevel == "Newfoundland and Labrador" ~ "Newfoundland",
      varlevel == "Prince Edward Island" ~ "PEI",
      
      # CMA
      varlevel == "Other CMA or non-CMA" ~ "Other CMA/non-CMA",
      varlevel == "Ottawa–Gatineau (Ontario part)" ~ "Ottawa–Gatineau",
      
      # Gender
      varlevel == "Male" ~ "Men",
      varlevel == "Female" ~ "Women",
      
      # Employment
      varlevel == "Single jobholder, including job changers" ~ "Single jobholder",
      varlevel == "Temporary, casual or other temporary jobs" ~ "Temporary, casual",
      varlevel == "Temporary, seasonal job" ~ "Temporary, seasonal",
      varlevel == "Temporary, term or contract job" ~ "Temporary, term/contract",
      
      varlevel == "Immigrant, landed 10 or less years earlier" ~ "Immigrant < 10 years",
      varlevel == "Immigrant, landed more than 10 years earlier" ~ "Immigrant > 10 years",
      
      # Youngest child
      varlevel == "Youngest child less than 6 years" ~ "Under 6 years",
      varlevel == "Youngest child 6 to 12 years" ~ "6-12 years",
      varlevel == "Youngest child 13 to 17 years" ~ "13-17 years",
      varlevel == "Youngest child 18 to 24 years" ~ "18-24 years",
      
      # Job tenure
      varlevel ==  "[1,12)" ~ "Under 1 year",
      varlevel ==  "[12,60)" ~ "1 to under 5",
      varlevel ==  "[60,120)" ~ "5 to under 10",
      varlevel ==  "[120,Inf]" ~ "10 years or more",
      
      # Industry
      varlevel == "Accommodation and food services" ~ "Accomodation/Food",
      varlevel == "Business, building and other support services" ~ "Business/Building/Other services",
      varlevel == "Educational services" ~ "Education",
      varlevel == "Finance and insurance" ~ "Finance/Insurance",
      varlevel == "Fishing, hunting and trapping" ~ "Fishing/Hunting/Trapping",
      varlevel == "Forestry and logging and support activities for forestry" ~ "Forestry/Support activities",
      varlevel == "Health care and social assistance" ~ "Health care/Social assistance",
      varlevel == "Information, culture and recreation" ~ "Information/Culture/Recreation",
      varlevel == "Manufacturing - durable goods" ~ "Manufacturing, durable",
      varlevel == "Manufacturing - non-durable goods" ~ "Manufacturing, non-durable",
      varlevel == "Mining, quarrying, and oil and gas extraction" ~ "Mining/Quarrying/Oil and gas",
      varlevel == "Other services (except public administration)" ~ "Other services",
      varlevel == "Professional, scientific and technical services" ~ "Professional/Scientific/Technical",
      varlevel == "Real estate and rental and leasing" ~ "Real estate/Rental/Leasing",
      varlevel == "Transportation and warehousing" ~ "Transportation/Warehousing",
      
      # Occupation
      varlevel == "Business, finance and administration occupations, except management" ~ 
        "Business/Finance/Admin",
      varlevel == "Health occupations, except management" ~ "Health",
      varlevel == "Management occupations" ~ "Management",
      varlevel == "Natural and applied sciences and related occupations, except management" ~ "Natural/Applied sciences",
      varlevel == "Natural resources, agriculture and related production occupations, except management" ~ "Natural resources/Agriculture/Related",
      varlevel == "Occupations in art, culture, recreation and sport, except management" ~ "Art/Culture/Recreation/Sport",
      varlevel == "Occupations in education, law and social, community and government services, except management" ~ 
        "Education/Law/SCG services",
      varlevel == "Occupations in manufacturing and utilities, except management" ~ "Manufacturing/Utilities",
      varlevel == "Sales and service occupations, except management" ~ "Sales/Service",
      varlevel == "Trades, transport and equipment operators and related occupations, except management" ~ "Trades/Transport/Equip operators",
      .default = as.character(varlevel)
    )
  ) |> 
  relocate(var_orig, .before = variable)
}

# Summarize the summary data from monthly to yearly
## Union density
density_data_prep <- function(df) {
  df |> 
  group_by(survyear, variable, varlevel, cowmain_rc, union_bin) |>
  summarise(p = mean(p, na.rm = TRUE),
            n = mean(n, na.rm = TRUE)) |>
  ungroup() |> 
  group_by(survyear, variable, varlevel) |> 
  mutate(p_pop = n / sum(n),
         unioncow = case_when(
           cowmain_rc == "Private sector" & union_bin == "Union member" ~ "Private (Unionized)",
           cowmain_rc == "Public sector" & union_bin == "Union member" ~ "Public (Unionized)",
           cowmain_rc == "Private sector" & union_bin == "Not member"  ~ "Private (No union)",
           cowmain_rc == "Public sector" & union_bin == "Not member"  ~ "Public (No union)",
         ),
         unioncow = factor(unioncow, 
                           levels = c('Public (Unionized)',
                                      'Private (Unionized)', 
                                      'Private (No union)', 
                                      'Public (No union)'))) |> 
  ungroup()
}

## Distribution of members
dist_data_prep <- function(df) {
  
  var_key <- df |> distinct(var_orig, variable)
  
  df |> 
  filter(union_bin == "Union member") |> 
  group_by(survyear, var_orig, varlevel, cowmain_rc) |>
  summarise(n = mean(n, na.rm = TRUE)) |>
  ungroup() |> 
  group_by(survyear, var_orig, cowmain_rc) |> 
  mutate(
    p_memb = n / sum(n),
    p_memb_bd = 1 - p_memb,
    cowmain_rc = as_factor(as.character(cowmain_rc))) |> 
  left_join(var_key) |> 
  relocate(variable, .after = var_orig) |> 
  ungroup()
}

