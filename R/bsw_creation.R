xfun::pkg_attach(c('tidyverse', 'targets', 'canpumf', 'janitor', 'zoo', 'here', 'qs', 'survey', 'svrep'))

# Labour force survey selection vector
bsw_vars <- c("ref_date", "survyear", "survmnth", "prov", "age_12", "sex", "finalwt")

lfs_df <- tar_read(lfs_prep)
lfs_sub <- lfs_df |> filter(survyear==2023) |> 
  select(rec_num, any_of(bsw_vars))

# https://policyalternatives.ca/sites/default/files/uploads/publications/National%20Office/2024/02/how-public-sector-is-fighting-income-inequality.pdf
# Poisson adjustment factor is 1 + PF * sqrt(finalwt-1/finalwt)
# BSW is finalwt * adjustment factor
# Calibrated BSW is sum(finalwt) by domain / sum(bsw) by domain * BSW
# Domains in this case are province, age, gender, by month


bsw_cal <- function(wt, bsw) { (sum(wt) /  sum(bsw)) * bsw}

## pfactor_add: Add poisson factor to dataframe
# Create a vector of random numbers with equal prob of being 1 or -1, with the 
# same length as the number of rows in the LFS dataframe, then bind cols

set.seed(2024)
pfactor_add <- function(df) {
  pf = sample(x = c(1,-1), size = nrow(df), prob = c(.5, .5), replace = TRUE)
  pf = as.data.frame(pf) |> as_tibble()
  df = df |> bind_cols(pf)
  df
}

## pfactor_adj: Calculate Poisson adjustment factor for each row of dataframe
# Adjustment factor: 1 + PF * sqrt(finalwt-1/finalwt)
# Use the poisson factor in the formula for the poisson adjustment factor

# pfactor_adj <- function(wt, pf) { 1 + pf * sqrt(wt-1/wt) }
pfactor_adj <- function(wt, pf) { 1 + pf * sqrt( (wt - 1) / wt) }

## bsw_uncal: Calculate bootstrap weight that is un-calibrated to domain/pseuro-pop
# final weight * adjustment factor
# pseudo-population, which is in the LFS ref date, prov, age, and gender

bsw_uncal <- function(wt, adj) {
  wt * adj
}

## bsw_cal: Calculate bootstrap weight calibrated to the domain/pseudo-pop
# sum(final weight) by domain / sum(bs weight) by domain * bs weight
# domains: survey month, province, age, gender

bsw_cal <- function(wt, bsw) {
  sum(wt) / sum(bsw) * bsw
}

lfs_sub |> 
  # Add poisson factor column to dataframe
  pfactor_add() |> 
  mutate(
    # Adjustment factor
    adj_factor = pfactor_adj(finalwt, pf),
    # Un-calibrated bootstrap weight
    bsw = bsw_uncal(finalwt, adj_factor)
  ) |> 
  group_by(ref_date, prov, age_12, sex) |> 
  mutate(bsw1 = bsw_cal(finalwt, bsw),
         across(bsw:bsw1, ~round(.x, digits = 2))) |> filter(pf==-1) |> arrange(-finalwt) #|> view()
  

# Debugging with chatgpt 1 --------------------------------------------------

# Function to apply the Poisson adjustment and calibration
apply_poisson_adjustment <- function(w, pf) {
  
  # Step 1: Compute the Poisson adjustment factor k
  k <- 1 + pf * sqrt((w - 1) / w)
  
  # Step 2: Calculate the uncalibrated bootstrap weight b
  b <- w * k
  
  return(list(k = k, b = b))
}

# Function to calibrate the bootstrap weights
calibrate_bootstrap_weight <- function(w, b, domains) {
  
  # Ensure the domains are properly defined in your data
  # domains should be a factor vector that groups the data by the domain variable (e.g., survey month, province, etc.)
  
  # Calculate sums of weights (w) and bootstrap weights (b) by domain
  sum_w_by_domain <- tapply(w, domains, sum)
  sum_b_by_domain <- tapply(b, domains, sum)
  
  # Step 3: Calibrate the bootstrap weight c for each domain
  # Calibration formula: c = (sum of w by domain / sum of b by domain) * b
  c <- (sum_w_by_domain[domains] / sum_b_by_domain[domains]) * b
  
  return(c)
}

# Sample data for testing (replace with your actual survey data)
# Survey weight (w), pf values, and domain variable (e.g., month, province)
w <- c(1.5, 2.0, 3.0, 4.5, 1.2)  # Survey weights
pf <- sample(c(1, -1), length(w), replace = TRUE)  # Randomly assign pf values of 1 or -1
domains <- factor(c("Month1", "Month2", "Month1", "Month2", "Month1"))  # Example domains (e.g., survey months)

# Apply Poisson adjustment and calculate uncalibrated bootstrap weights
adjustment <- apply_poisson_adjustment(w, pf)

# Calibrate the bootstrap weights
c <- calibrate_bootstrap_weight(w, adjustment$b, domains)

# Display results
print("Poisson Adjustment Factor (k):")
print(adjustment$k)

print("Uncalibrated Bootstrap Weights (b):")
print(adjustment$b)

print("Calibrated Bootstrap Weights (c):")
print(c)


# GPT - tidy style --------------------------------------------------------

library(tidyverse)

# Sample data for testing (replace with your actual survey data)
# Create a data frame with survey weights (w), pf values, and domain variable (e.g., month, province)
survey_data <- tibble(
  w = c(1.5, 2.0, 3.0, 4.5, 1.2),  # Survey weights
  pf = sample(c(1, -1), 5, replace = TRUE),  # Randomly assign pf values of 1 or -1
  domain = factor(c("Month1", "Month2", "Month1", "Month2", "Month1"))  # Example domains (e.g., survey months)
)

# Apply Poisson adjustment, uncalibrated bootstrap weight, and calibration
survey_data <- survey_data %>%
  # Step 1: Calculate the Poisson adjustment factor k
  mutate(k = 1 + pf * sqrt((w - 1) / w)) %>%
  
  # Step 2: Calculate the uncalibrated bootstrap weight b
  mutate(b = w * k) %>%
  
  # Step 3: Sum of w and b by domain for calibration
  group_by(domain) %>%
  mutate(
    sum_w = sum(w),       # Total survey weight per domain
    sum_b = sum(b)        # Total bootstrap weight per domain
  ) %>%
  
  # Step 4: Calibrate the bootstrap weight c
  mutate(c = (sum_w / sum_b) * b) %>%
  
  # Remove intermediate summary columns for clarity
  ungroup() %>%
  select(-sum_w, -sum_b)

# View the final output
print(survey_data)

## Adapting the sample code
# Apply Poisson adjustment, uncalibrated bootstrap weight, and calibration
survey_data <- lfs_sub %>%
  pfactor_add() %>%
  # Step 1: Calculate the Poisson adjustment factor k
  mutate(k = 1 + pf * sqrt((finalwt - 1) / finalwt)) %>%
  
  # Step 2: Calculate the uncalibrated bootstrap weight b
  mutate(bsw = finalwt * k) %>%
  
  # Step 3: Sum of w and b by domain for calibration
  group_by(ref_date, prov, age_12, sex) %>%
  mutate(
    sum_w = sum(finalwt),       # Total survey weight per domain
    sum_b = sum(bsw)        # Total bootstrap weight per domain
  ) %>%
  
  # Step 4: Calibrate the bootstrap weight c
  mutate(bsw1 = (sum_w / sum_b) * bsw) %>%
  
  # Remove intermediate summary columns for clarity
  ungroup() %>%
  select(-sum_w, -sum_b)

# View the final output
print(survey_data)

survey_data |> 
  mutate(across(bsw:bsw1, ~round(.x, digits = 2))) |> 
  arrange(-finalwt)
