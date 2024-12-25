xfun::pkg_attach("tidyverse", "here", "janitor", "clipr", "DatawRappr", "zoo", "glue")

# Setup -------------------------------------------------------------------

## Libraries, theme, colours
source(here::here('R', 'dash_setup.R'))

## Factor levels and selector order
source(here::here('R', 'dash_levels.R')) 

# Data --------------------------------------------------------------------

## Density data
dens_df <- tar_read(density_df) |> 
  filter(union_bin == "Union member") |> 
  mutate(p = ifelse(is.nan(p), NA, p),
         # Normalize cowmain_rc
         cowmain_rc = as.character(cowmain_rc),
         cowmain_rc = str_trim(cowmain_rc),
         cowmain_rc = str_to_lower(cowmain_rc)
         ) |> 
  select(survyear, variable, varlevel, cowmain_rc, p)


# Functions ---------------------------------------------------------------


# Function to prepare data for each variable and sector
prep_data_for_sectors <- function(data, variable) {

  # Filter and pivot data
  filtered_data <- data |> filter(variable == !!variable)
  
  if (nrow(filtered_data) == 0) {
    print(paste("No data for variable:", variable))
    return(NULL)
  }
  
  # Reorder varlevel based on factor_levels
  levels <- factor_levels |> 
    filter(variable == !!variable) |> 
    pull(varlevel)
  
  if (length(levels) == 0) {
    print(paste("No levels found for variable:", variable))
    return(NULL)
  }
  
  filtered_data <- filtered_data |> 
    mutate(varlevel = factor(varlevel, levels = levels))
  
  # Pivot data
  prepared_data <- filtered_data |> 
    arrange(varlevel) |> 
    pivot_wider(
      id_cols = c(survyear, cowmain_rc),
      names_from = varlevel,
      values_from = p,
      values_fn = \(x) x * 100
    )
  
  # Split by sector
  list(
    `public sector` = prepared_data |> filter(cowmain_rc == "public sector"),
    `private sector` = prepared_data |> filter(cowmain_rc == "private sector")
  )
}

# Function to create annotations for a specific variable and sector
create_annotations <- function(data, variable, sector, color) {
  anno_df <- data |> 
    filter(variable == !!variable, cowmain_rc == !!sector, 
           survyear %in% seq(2006, 2024, by = 3)) |> 
    mutate(
      dy = -15,
      align = "mc",
      size = 14,
      bg = TRUE,
      bold = TRUE,
      mobileFallback = FALSE,
      showMobile = FALSE,
      showDesktop = TRUE,
      connectorLine = list(list(enabled = FALSE)),
      text = scales::percent(p, accuracy = .1),
      color = color,
      p = p * 100,
      showInAllPlots = FALSE
    ) |>   
    rowwise() |> 
    mutate(position = list(list(x = sprintf("%s-01-01", survyear), y = p, plot = varlevel))) |> 
    ungroup() |>
    select(
      text,
      position,
      dy,
      color,
      bold,
      showMobile,
      showDesktop,
      bg,
      mobileFallback,
      connectorLine,
      align,
      size,
      showInAllPlots
    )
  
  # Convert annotations to a nested list
  vlabel_anno <- anno_df %>%
    pmap(~ setNames(list(...), names(anno_df)))
  
  # Create point annotations
  point_anno_df <- anno_df |> 
    select(-bg) |> 
    mutate(
      text = "⬤",
      size = 11,
      dy = 0,
      showMobile = TRUE
    )
  
  points_anno <- point_anno_df %>%
    pmap(~ setNames(list(...), names(point_anno_df)))
  
  # Combine annotations
  c(vlabel_anno, points_anno)
}

# Function to create a chart for a specific sector
create_chart <- function(data, variable, sector, folder_id, annotations) {
  color <- if (sector == "public sector") "#2E20E0" else "#136BDC"
  
  # Create chart
  chart <- dw_create_chart(
    title = paste("Union Density by", variable, "-", sector),
    folderId = folder_id,
    type = "multiple-lines"
  )
  chart_id <- chart[["id"]]
  
  # Upload data
  dw_data_to_chart(data, chart_id)
  
  # Customize chart
  dw_edit_chart(
    
    # Describe
    chart_id = chart_id,
    title = paste0("<span style='color:",color,";'>",str_to_upper(sector),"</span>"),
    intro = paste("Union density (% union membership) by", variable),
    byline = "Ryan Romard",
    source_name = "LFS PUMF (2006-2024)",
    source_url = "https://www150.statcan.gc.ca/n1/en/catalogue/71M0001X",
    
    # Axes
    axes = list(y="p", x= "survyear"),
    
    # Viz
    visualize = list(
      
      # Layout
      gridLayout = "minimumWidth",
      gridColumnMinWidth = 300,
      gridColumnCountMobile = 2,
      gridRowHeightMode = "fixed",
      gridRowHeightFixed = 250,
      gridRowHeightRatio = 1,
      plotHeightFixed = 300,
      plotHeightMode = "fixed",
      
      ## Y-axis
      `y-grid-on` = TRUE,
      `y-grid-format` = "0%",
      `y-grid-labels` = "inside",
      `custom-range-y` = c(0,100),
      `custom-ticks-y` = "0,25,50,75,100",
      `y-grid-subdivide` = TRUE,
      yGridLabelAllColumns = TRUE,
      xGridLabelAllColumns = TRUE,
      
      ## X-axis
      `x-grid-on` = TRUE,

      `x-grid-format` = "auto",
      `custom-range-x` = c(2005,2025),
      `custom-ticks-x` = "2006,2009,2012,2015,2018,2021,2024",
      
      # Colors
      `base-color` = color,
      `show-color-key`= FALSE,
      `variate-colors` = FALSE,  ## ??? what does this do
      
      # Shapes, lines, fills
      ## Lines
      interpolation = "monotone-x",
      
      ## Fill under line
      areaFill = list(
        to = "--zero-baseline--",
        color = 0,
        enabled = TRUE,
        opacity = 0.5,
        useLineColor = TRUE,
        useMixedColors = FALSE
      ),
      
      # Annotations, tool-tips
      ## Anno
      `text-annotations` = annotations,
      ## Tooltips
      `show-tooltips` = FALSE,
      `tooltip-number-format` = "0.0%",
      `tooltip-x-format` = "YYYY"
      
      # Publish
      
    )
  )
  
  # Publish chart
  dw_publish_chart(chart_id)
  
  return(chart_id)
}

test1 <- dw_retrieve_chart_metadata("8aaJN")
test1$content$metadata$visualize$`x-grid-on`

# Table styling
dwdash_tab <- function(dw,color,sector) {
  dw_edit_chart(
    dw,
    type = "tables,"
    # Data
    "data" = list(
      "column-format" = list(
        "variable" = list("ignore" = TRUE),
        "cowmain_rc" = list("ignore" = TRUE),
        list(id = "6jf4Jmc8qP",row = 0,column = 0,value = "Year",previous = "survyear",ignored = FALSE)
      )
    ),
    # Describe
    title = paste0("<span style='color:",color,";'>",str_to_upper(sector),"</span>"),
    intro = paste("Union density (% union membership) by", variable),
    byline = "Ryan Romard",
    source_name = "LFS PUMF (2006-2024)",
    source_url = "https://www150.statcan.gc.ca/n1/en/catalogue/71M0001X",
    # Visuals
    visualize = list(
      # Layout
      compactMode = TRUE,
      mobileFallback = TRUE,
      firstColumnIsSticky = TRUE,
      striped = TRUE,
      header = list(style = list(background = color))
    ),
    # Publish
    "publish" = list("embed-width" = 900)
  )
}

dw_retrieve_chart_metadata("E5SiV")

# Iteration ---------------------------------------------------------------

# Variables and sectors
variable_order <- c(
  "Province",
  "CMA",
  "Age group",
  "Gender",
  "Family type",
  "Youngest child",
  "Education level",
  "Student status",
  "Immigration",
  "Occupation",
  "Industry",
  "Establishment size",
  "Firm size",
  "Employment status",
  "Job tenure"
  )

variables <- dens_df |> dplyr::distinct(variable) |> pull() |> as.character()
variables <- variables[order(match(variables, dash_vars_order))]

folder_id <- 285422
folder_df <- tribble(~variable, ~folder_id,
                     "Province", 285510,
                     "CMA", 285511,
                     "Age group", 285512,
                     "Gender", 285513,
                     "Family type", 285514,
                     "Youngest child", 285515,
                     "Education level", 285516,
                     "Student status", 285517,
                     "Immigration", 285518,
                     "Occupation", 285519,
                     "Industry", 285520,
                     "Establishment size", 285521,
                     "Firm size", 285522,
                     "Employment status", 285523,
                     "Job tenure", 285524,
)

# Iterate over variables and sectors
chart_ids <- map(variables, function(variable) {
  print(paste("Processing variable:", variable))
  
  # Prepare data
  data_split <- prep_data_for_sectors(dens_df, variable)
  
  if (is.null(data_split)) {
    return(NULL)
  }
  
  # Get folder ID
  folder_id <- folder_df |> 
    filter(variable == !!variable) |> 
    pull(folder_id)
  
  if (is.null(folder_id) || length(folder_id) == 0) {
    print(paste("No folder ID for variable:", variable))
    return(NULL)
  }
  
  # Create charts for each sector
  sectors <- c("public sector", "private sector")
  map(sectors, function(sector) {
    sector_data <- data_split[[sector]]
    
    if (is.null(sector_data) || nrow(sector_data) == 0) {
      print(paste("No data for", sector, "for variable:", variable))
      return(NULL)
    }

    # Generate annotations
    color <- if (sector == "public sector") "#2E20E0" else "#136BDC"
    annotations <- create_annotations(dens_df, variable, sector, color)
    
    # Create chart
    create_chart(sector_data, variable, sector, folder_id, annotations)
  })
})

create_table <- function(data, variable, sector, folder_id) {
  # Define sector-specific color
  color <- if (sector == "public sector") "#2E20E0" else "#136BDC"
  
  # Create a new table
  table <- dw_create_chart(
    folderId = folder_id,
    type = "tables"
  )
  table_id <- table[["id"]]
  
  # Upload data
  dw_data_to_chart(data, table_id)
  
  # Edit and style the table
  dwdash_tab(table_id, color, sector)
  
  # Publish the table
  dw_publish_chart(table_id)
  
  return(table_id)
}

# variables
variables <- dens_df |> dplyr::distinct(variable) |> pull() |> as.character()
variables <- variables[order(match(variables, dash_vars_order))]

# Create tables for each variable and sector
table_ids <- map(variables, function(variable) {
  print(paste("Processing variable:", variable))
  
  # Prepare data
  data_split <- prep_data_for_sectors(dens_df, variable)
  
  if (is.null(data_split)) {
    print(paste("No data for variable:", variable))
    return(NULL)
  }
  
  # Retrieve folder ID
  folder_id <- folder_df |> 
    filter(variable == !!variable) |> 
    pull(folder_id)
  
  if (is.null(folder_id) || length(folder_id) == 0) {
    print(paste("No folder ID for variable:", variable))
    return(NULL)
  }
  
  # Create tables for each sector
  sectors <- c("public sector", "private sector")
  map(sectors, function(sector) {
    sector_data <- data_split[[tolower(sector)]]
    
    if (is.null(sector_data) || nrow(sector_data) == 0) {
      print(paste("No data for", sector, "for variable:", variable))
      return(NULL)
    }
    
    create_table(sector_data, variable, sector, folder_id)
  })
})

dw_retrieve_chart_metadata("K39vf")

folders <- dw_list_folders() 
folders

dw_list_charts()


# Accessing chart metadata for JS selection -------------------------------
library(httr)
library(jsonlite)

# Code block for dropdown selector
<div>
  <label for="variable-select">Select a Variable:</label>
  <select id="variable-select" onchange="updateContent()"> 
  <option value="province">Province</option>
  <option value="cma">CMA</option>
  <option value="age-group">Age group</option>
  <option value="gender">Gender</option>
  <option value="family-type">Family type</option>
  <option value="youngest-child">Youngest child</option>
  <option value="education-level">Education level</option>
  <option value="student-status">Student status</option>
  <option value="immigation">Immigration</option>
  <option value="occupation">Occupation</option>
  <option value="industry">Industry</option>
  <option value="establishment-size">Establishment size</option>
  <option value="firm-size">Firm size</option>
  <option value="employment-status">Employment status</option>
  <option value="job-tenure">Job tenure</option>
  <!-- Add more options as needed --> </select>
  </div>

# Function to query Datawrapper charts manually
dw_list_charts_manual <- function(api_key, limit = 100) {
  base_url <- "https://api.datawrapper.de/v3/charts"
  
  response <- GET(
    base_url,
    add_headers(Authorization = paste("Bearer", api_key)),
    query = list(order = "DESC", orderBy = "createdAt", limit = limit)
  )
  
  if (response$status_code == 200) {
    # Parse and return the content
    content(response, "text", encoding = "UTF-8") |>
      fromJSON(flatten = TRUE) |>
      as.data.frame()
  } else {
    stop("Failed to retrieve Datawrapper charts. Check your API key or connection.")
  }
}



chart_list <- dw_list_charts_manual(dw_get_api_key(), limit = 100)
folder_list <- dw_list_folders()

folder_id <- 285422
folder_df <- tribble(~variable, ~folder_id, ~var_sel,
                     "Province", 285510, "province",
                     "CMA", 285511, "cma",
                     "Age group", 285512, "age-group",
                     "Gender", 285513, "gender",
                     "Family type", 285514, "family-type",
                     "Youngest child", 285515, "youngest-child",
                     "Education level", 285516, "education-level",
                     "Student status", 285517, "student-status",
                     "Immigration", 285518, "immigration",
                     "Occupation", 285519, "occupation",
                     "Industry", 285520, "industry",
                     "Establishment size", 285521, "establishment-size",
                     "Firm size", 285522, "firm-size",
                     "Employment status", 285523, "employment-status",
                     "Job tenure", 285524, "job-tenure"
)

# Extract a list of folders and charts from DW manually
dw_list_charts_manual <- function(api_key, limit = 100) {
  base_url <- "https://api.datawrapper.de/v3/charts"
  
  response <- GET(
    base_url,
    add_headers(Authorization = paste("Bearer", api_key)),
    query = list(order = "DESC", orderBy = "createdAt", limit = limit)
  )
  
  if (response$status_code == 200) {
    # Parse and return the content
    content(response, "text", encoding = "UTF-8") |>
      fromJSON(flatten = TRUE) |>
      as.data.frame()
  } else {
    stop("Failed to retrieve Datawrapper charts. Check your API key or connection.")
  }
}

# Turn it into a usable dataframe
chart_list <- dw_list_charts_manual(dw_get_api_key(), limit = 200) |> as_tibble()
chart_meta_df <- chart_list |> 
  filter(list.folderId %in% folder_df$folder_id) |> 
  select(
    folder_id = list.folderId,
    chart_title = list.title,
    chart_type = list.type,
    chart_id = list.id,
    chart_stem = list.url,
    chart_url = list.publicUrl
  ) |> 
  arrange(folder_id) |> 
  left_join(folder_df) |> 
  relocate(variable, var_sel, .before = 1)

# Isolate the needed information for each chart
var_select_df <- chart_meta_df |> 
  mutate(
    sector = ifelse(str_detect(chart_title, "PUBLIC"), "public", "private")
  ) |> 
  select(var_sel, sector, chart_type, chart_id)

# Cast chart IDs to json
chartIds <- var_select_df %>%
  filter(chart_type == "multiple-lines") %>%
  group_by(var_sel) %>%
  summarise(
    public = chart_id[sector == "public"],
    private = chart_id[sector == "private"],
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(data = list(list(public = public, private = private))) %>%
  ungroup() %>%
  select(var_sel, data) %>%
  deframe()

# Convert to JSON without arrays
chartIds_json <- toJSON(chartIds, pretty = TRUE, auto_unbox = TRUE)

# View JSON
cat(chartIds_json)

# Cast table IDs to json
tableIds <- var_select_df %>%
  filter(chart_type == "tables") %>%
  group_by(var_sel) %>%
  summarise(
    public = chart_id[sector == "public"],
    private = chart_id[sector == "private"],
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(data = list(list(public = public, private = private))) %>%
  ungroup() %>%
  select(var_sel, data) %>%
  deframe()

# Convert to JSON without arrays
tableIds_json <- toJSON(tableIds, pretty = TRUE, auto_unbox = TRUE)

# View JSON
cat(tableIds_json)

```{=html}
<script>
  const chartIds = {
    
    "education-level": ["PKwgQ", "fJEdR"],
    "age-group": { public: "e9R8c", private: "SiZ3B" },
    "gender": { public: "Qvury", private: "j1oYG" }
    
  };

const tableIds = {
  "education-level": { public: "Mm8vM", private: "fyj4k" },
  "age-group": { public: "E5SiV", private: "10TGY" },
  "gender": { public: "xQsIS", private: "zAHKb" }
};

function updateContent() {
  const variable = document.getElementById("variable-select").value;
  
  // Clear and update Public Sector Chart
  const publicChartContainer = document.getElementById("public-sector-chart");
  publicChartContainer.innerHTML = ""; // Clear previous content
  const publicScript = document.createElement("script");
  publicScript.type = "text/javascript";
  publicScript.defer = true;
  publicScript.src = `https://datawrapper.dwcdn.net/${chartIds[variable].public}/embed.js`;
  publicScript.setAttribute("charset", "utf-8");
  publicScript.setAttribute("data-target", `#datawrapper-vis-${chartIds[variable].public}`);
  const publicDiv = document.createElement("div");
  publicDiv.style.minHeight = "400px";
  publicDiv.id = `datawrapper-vis-${chartIds[variable].public}`;
  publicDiv.appendChild(publicScript);
  publicChartContainer.appendChild(publicDiv);
  
  // Clear and update Private Sector Chart
  const privateChartContainer = document.getElementById("private-sector-chart");
  privateChartContainer.innerHTML = ""; // Clear previous content
  const privateScript = document.createElement("script");
  privateScript.type = "text/javascript";
  privateScript.defer = true;
  privateScript.src = `https://datawrapper.dwcdn.net/${chartIds[variable].private}/embed.js`;
  privateScript.setAttribute("charset", "utf-8");
  privateScript.setAttribute("data-target", `#datawrapper-vis-${chartIds[variable].private}`);
  const privateDiv = document.createElement("div");
  privateDiv.style.minHeight = "400px";
  privateDiv.id = `datawrapper-vis-${chartIds[variable].private}`;
  privateDiv.appendChild(privateScript);
  privateChartContainer.appendChild(privateDiv);
  
  // Clear and update Public Sector Table
  const publicTableContainer = document.getElementById("public-sector-table");
  publicTableContainer.innerHTML = ""; // Clear previous content
  const publicTableScript = document.createElement("script");
  publicTableScript.type = "text/javascript";
  publicTableScript.defer = true;
  publicTableScript.src = `https://datawrapper.dwcdn.net/${tableIds[variable].public}/embed.js`;
  publicTableScript.setAttribute("charset", "utf-8");
  publicTableScript.setAttribute("data-target", `#datawrapper-vis-${tableIds[variable].public}`);
  const publicTableDiv = document.createElement("div");
  publicTableDiv.style.minHeight = "400px";
  publicTableDiv.id = `datawrapper-vis-${tableIds[variable].public}`;
  publicTableDiv.appendChild(publicTableScript);
  publicTableContainer.appendChild(publicTableDiv);
  
  // Clear and update Private Sector Table
  const privateTableContainer = document.getElementById("private-sector-table");
  privateTableContainer.innerHTML = ""; // Clear previous content
  const privateTableScript = document.createElement("script");
  privateTableScript.type = "text/javascript";
  privateTableScript.defer = true;
  privateTableScript.src = `https://datawrapper.dwcdn.net/${tableIds[variable].private}/embed.js`;
  privateTableScript.setAttribute("charset", "utf-8");
  privateTableScript.setAttribute("data-target", `#datawrapper-vis-${tableIds[variable].private}`);
  const privateTableDiv = document.createElement("div");
  privateTableDiv.style.minHeight = "400px";
  privateTableDiv.id = `datawrapper-vis-${tableIds[variable].private}`;
  privateTableDiv.appendChild(privateTableScript);
  privateTableContainer.appendChild(privateTableDiv);
}

// Initialize the dashboard on page load
document.addEventListener("DOMContentLoaded", function() {
  document.getElementById("variable-select").value = "education-level"; // Default variable
  updateContent();
});
</script>
  ```
