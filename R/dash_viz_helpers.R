# Helper for chart and output height
calculate_height <- function(layout, variable, measure, dataset) {
  if (layout %in% c("Vertical", "Horizontal")) {
    return(500 * (dataset |> dplyr::distinct(varlevel) |> nrow() / 2))
  }
  if (layout == "Stacked") {
    if (variable != "Employment status" & measure != "Union density") {
      return(650)
    }
    if (variable == "Employment status" & measure != "Union density") {
      return(1200)
    }
    if (measure == "Union density") {
      return(500 * (dataset |> dplyr::distinct(varlevel) |> nrow() / 2))
    }
  }
  return(500) # Default height
}

# Helper for determining number of rows
determine_num_rows <- function(variable) {
  case_when(
    variable %in% c("Gender", "Immigration", "Age group", "Student status") ~ 1,
    variable %in% c(
      "Education level", "Family type", "Youngest child", "Job tenure", "Firm size", "Establishment size"
    ) ~ 2,
    variable %in% c("Province", "CMA", "Occupation", "Employment status") ~ 4,
    variable %in% c("Industry") ~ 7,
    TRUE ~ 1 # Default value
  )
}

# Helper for determining area margins
determine_margins <- function(variable) {
  case_when(
    variable %in% c("Gender") ~ 75,
    variable %in% c("Job tenure", "Age group", "Family type", "Youngest child", "Student status") ~ 130,
    variable %in% c(
      "Province", "CMA", "Education level", "Immigration", "Employment status", "Firm size", "Establishment size"
    ) ~ 150,
    variable %in% c("Industry", "Occupation") ~ 270,
    TRUE ~ 100 # Default margin
  )
}

# Table preparation
prepare_kable <- function(df, measure) {
  variable <- df |> dplyr::distinct(variable) |> dplyr::pull(variable) |> as.character()
  if (measure == "Union density") {
    df <- df |> dplyr::filter(union_bin == "Union member")
  }
  df |> dplyr::mutate(variable = variable)
}
