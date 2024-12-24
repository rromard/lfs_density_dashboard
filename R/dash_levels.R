# Dataframe full of default factor levels
factor_levels <-
  tibble::tribble(
    ~ varlevel,
    ~ variable,
    
    # Age groups
    "65+","Age group",
    "55 to 64","Age group",
    "25 to 54","Age group",
    "15 to 24","Age group",
    
    # CMA
    "Calgary","CMA",
    "Edmonton","CMA",
    "Hamilton","CMA",
    "Montréal","CMA",
    "Ottawa–Gatineau","CMA",
    "Québec","CMA",
    "Toronto","CMA",
    "Vancouver","CMA",
    "Winnipeg","CMA",
    "Other CMA/non-CMA","CMA",
    
    # Province
    "Alberta","Province",
    "British Columbia","Province",
    "Manitoba","Province",
    "New Brunswick","Province",
    "Newfoundland","Province",
    "Nova Scotia","Province",
    "Ontario","Province",
    "PEI","Province",
    "Quebec","Province",
    "Saskatchewan","Province",
    
    # Education
    "Graduate","Education level",
    "Undergraduate","Education level",
    "College","Education level",
    "Secondary/Some PSE","Education level",
    "Under secondary","Education level",
    
    # Student status
    "Part-time student","Student status",
    "Full-time student","Student status",
    "Non-student","Student status",
    
    # Family type
    "Other families","Family type",
    "Lone-parent","Family type",
    "Not in econ family","Family type",
    "Single-earner","Family type",
    "Dual-earner","Family type",
    
    # Gender
    "Men","Gender",
    "Women","Gender",
    
    # Immigration
    "Immigrant < 10 years","Immigration",
    "Immigrant > 10 years","Immigration",
    "Non-immigrant","Immigration",
    
    # Employment
    "Standard employment","Employment status",
    "Non-standard employment","Employment status",
    "Full-time","Employment status",
    "Part-time","Employment status",
    "Multiple jobholder","Employment status",
    "Single jobholder","Employment status",
    "Permanent","Employment status",
    "Temporary, casual","Employment status",
    "Temporary, seasonal","Employment status",
    "Temporary, term/contract","Employment status",
    
    # Job tenure
    "10 years or more","Job tenure",
    "5 to under 10","Job tenure",
    "1 to under 5","Job tenure",
    "Under 1 year","Job tenure",
    
    # Industry
    "Accomodation/Food","Industry",
    "Agriculture","Industry",
    "Business/Building/Other services","Industry",
    "Construction","Industry",
    "Education","Industry",
    "Finance/Insurance","Industry",
    "Fishing/Hunting/Trapping","Industry",
    "Forestry/Support activities","Industry",
    "Health care/Social assistance","Industry",
    "Information/Culture/Recreation","Industry",
    "Manufacturing, durable","Industry",
    "Manufacturing, non-durable","Industry",
    "Mining/Quarrying/Oil and gas","Industry",
    "Other services","Industry",
    "Professional/Scientific/Technical","Industry",
    "Public administration","Industry",
    "Real estate/Rental/Leasing","Industry",
    "Retail trade","Industry",
    "Transportation/Warehousing","Industry",
    "Utilities","Industry",
    "Wholesale trade","Industry",
    
    # Firm size
    "More than 500 employees", "Firm size",
    "100 to 500 employees","Firm size",
    "20 to 99 employees","Firm size",
    "Less than 20 employees","Firm size",
    
    # Est size
    "More than 500 employees","Establishment size",
    "100 to 500 employees","Establishment size",
    "20 to 99 employees","Establishment size",
    "Less than 20 employees","Establishment size",
    
    # Occupation
    "Business/Finance/Admin","Occupation",
    "Health","Occupation",
    "Management","Occupation",
    "Natural/Applied sciences","Occupation",
    "Natural resources/Agriculture/Related","Occupation",
    "Art/Culture/Recreation/Sport","Occupation",
    "Education/Law/SCG services","Occupation",
    "Manufacturing/Utilities","Occupation",
    "Sales/Service","Occupation",
    "Trades/Transport/Equip operators","Occupation",
    
    # Youngest child
    "18-24 years","Youngest child",
    "13-17 years","Youngest child",
    "6-12 years","Youngest child",
    "Under 6 years","Youngest child",
    "No children","Youngest child"
  )

# Set selector list order
dash_vars_order <- c(
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
  "Job tenure")
