#Feminist Replication - "The Wages of Care: Bargaining Power, Earnings and Inequality"
# Efforts By : Rhythm Vats(MAEC1240025)

#Setting the directory
setwd()

#Uploading data
data<- read_dta("cps_00010.dta")
View(data)
#Installing and uploading packages in library
install.packages("labelled")
install.packages("haven")
install.packages("dplyr")
install.packages("dbplyr")
install.packages("stringr")
install.packages("survey")
install.packages("srvyr")
install.packages("modelsummary")
install.packages("estimatr")
install.packages("knitr")
library(labelled)
library(haven)
library(dplyr)
library(dbplyr)
library(stringr)
library(survey)    
library(srvyr)     
library(modelsummary) # helps to make good regression tables
library(estimatr)
library(knitr)
library(scales)
library(tidyr) 
library(broom)
library(scales)

data <- data %>% filter(asecflag == 1)

names(data)

#Naming sex variable as Male for 1 and Female for 2
categorical_vars <- c("sex", "marst", "empstat", "classwkr", "educ", "hispan", "race")

# Apply as_factor() to only the listed variables
data <- data %>%
  mutate(across(all_of(categorical_vars), haven::as_factor))

#Replacing NIU Values to NA if it is exists 
replace_niu_all <- function(df, vars = NULL) {
  if(is.null(vars)) vars <- names(df)
  for(v in vars) {
    if(!v %in% names(df)) next
    if(is.factor(df[[v]]) || is.character(df[[v]])) {
      ch <- as.character(df[[v]])
      ch[grepl("niu|not in universe|niu or blank", ch, ignore.case = TRUE)] <- NA_character_
      df[[v]] <- droplevels(factor(ch))
    }
  }
  df
}

#Apply to all variables 
vars_to_clean <- c("sex","empstat","classwkr","marst","educ","uhrsworkt","earnweek")
data <- replace_niu_all(data, vars_to_clean)

#combining race and ethnicity

# 1) Detect likely race/hisp variables 
find_var <- function(patterns, df) {
  cols <- names(df)
  for(p in patterns) {
    found <- cols[str_detect(tolower(cols), tolower(p))]
    if(length(found)) return(found[1])
  }
  return(NA_character_)
}

race_var <- find_var(c("race","race1","raceth","racer"), data)
hisp_var <- find_var(c("hispan","hisp","hispanic","ethnic"), data)
cat("Detected race var:", race_var, "\nDetected hispanic var:", hisp_var, "\n\n")

# 2) Print unique values to inspect (first 100)
if(!is.na(race_var)) {
  cat("Unique (sample) values of race variable:\n")
  print(unique(as.character(data[[race_var]]))[1:100])
} else cat("No race var auto-detected.\n")

if(!is.na(hisp_var)) {
  cat("\nUnique (sample) values of hispanic variable:\n")
  print(unique(as.character(data[[hisp_var]]))[1:100])
} else cat("No hispanic var auto-detected.\n")

# 3) Robust mapping: handle numeric-coded or text-coded versions
data <- data %>%
  mutate(
    race_ch = if(!is.na(race_var)) as.character(.data[[race_var]]) else NA_character_,
    hisp_ch = if(!is.na(hisp_var)) as.character(.data[[hisp_var]]) else NA_character_,
    
    # Normalize HISPAN: common numeric codes or text labels
    hisp_flag = case_when(
      # numeric codes often used: 0/1 or 1=Hispanic etc. Try to coerce if purely numeric-like
      !is.na(hisp_ch) & str_detect(hisp_ch, "^[0-9]+$") & as.numeric(hisp_ch) %in% c(1) ~ "Hispanic",
      !is.na(hisp_ch) & str_detect(hisp_ch, "^[0-9]+$") & as.numeric(hisp_ch) %in% c(0) ~ "Not Hispanic",
      # textual matches
      !is.na(hisp_ch) & str_detect(tolower(hisp_ch), "hispanic|latino|mexican|puerto|cuban|dominican") ~ "Hispanic",
      !is.na(hisp_ch) & str_detect(tolower(hisp_ch), "not hispanic|not hispanic or latino|no") ~ "Not Hispanic",
      TRUE ~ NA_character_
    ),
    
    # Normalize race names with many variants
    race_key = case_when(
      !is.na(race_ch) & str_detect(tolower(race_ch), "white") ~ "White",
      !is.na(race_ch) & str_detect(tolower(race_ch), "black|african") ~ "Black",
      !is.na(race_ch) & str_detect(tolower(race_ch), "asian") ~ "Asian",
      !is.na(race_ch) & str_detect(tolower(race_ch), "american indian|alaska|native") ~ "Native",
      !is.na(race_ch) & str_detect(tolower(race_ch), "two or more|multiracial|other") ~ "Other",
      TRUE ~ NA_character_
    ),
    
    # Now combine into the 4 categories used in the paper:
    race_eth = case_when(
      hisp_flag == "Hispanic" ~ "Hispanic",
      hisp_flag == "Not Hispanic" & race_key == "White" ~ "White, non-Hispanic",
      hisp_flag == "Not Hispanic" & race_key == "Black" ~ "Black, non-Hispanic",
      hisp_flag == "Not Hispanic" & !is.na(race_key) ~ "Other, non-Hispanic",
      is.na(hisp_flag) & race_key == "White" ~ "White, non-Hispanic",
      is.na(hisp_flag) & race_key == "Black" ~ "Black, non-Hispanic",
      is.na(hisp_flag) & !is.na(race_key) ~ "Other, non-Hispanic",
      TRUE ~ NA_character_
    ),
    
    race_eth = factor(race_eth, levels = c("White, non-Hispanic","Black, non-Hispanic","Other, non-Hispanic","Hispanic"))
  )

# 4) Show counts
cat("\nResulting race_eth distribution (unweighted counts):\n")
print(table(data$race_eth, useNA = "ifany"))

# If still mostly Hispanics, print examples where race_key is NA but hisp_flag indicates Not Hispanic
if(sum(!is.na(data$hisp_flag) & data$hisp_flag == "Not Hispanic" & is.na(data$race_key), na.rm=TRUE) > 0) {
  cat("\nExamples where HISPAN says Not Hispanic but race_key is NA (first 20):\n")
  print(head(data %>% filter(hisp_flag == "Not Hispanic" & is.na(race_key)) %>% select(race_ch, hisp_ch) , 20))
}

#keeping the age to be equal to more than 15 and till age 66 excluding self employed or someone with no earnings

data <- data %>%
  # 1. Convert age once to a new numeric variable, handling labels/codes as NA
  mutate(
    age = suppressWarnings(as.numeric(as.character(age)))
  ) %>%
  # 2. Filter out missing/non-numeric ages
  filter(!is.na(age)) %>%
  # 3. Apply the age filter
  filter(age >= 15, age <=66)

is.numeric(data$age)

# empstat command 
data <- data %>%
filter(
  # Convert to character, lowercase, then search for keywords
  str_detect(tolower(as.character(empstat)), "at work|has job|armed forces") 
)

#Computing inc wage 
data <- data %>%
  mutate(
    # A. Annual Wage (INCWAGE)
    annual_wage = suppressWarnings(as.numeric(as.character(incwage))), 
    
    # B. WKSWORK2 Midpoint Imputation
    weeks_worked_mid = case_when(
      as.numeric(as.character(wkswork2)) == 1 ~ 7,
      as.numeric(as.character(wkswork2)) == 2 ~ 20,
      as.numeric(as.character(wkswork2)) == 3 ~ 33,
      as.numeric(as.character(wkswork2)) == 4 ~ 43.5,
      as.numeric(as.character(wkswork2)) == 5 ~ 48.5,
      as.numeric(as.character(wkswork2)) == 6 ~ 51,
      TRUE ~ NA_real_ 
    ),
    
    # C. Calculate Weekly Wage
    weekly_wage = annual_wage / weeks_worked_mid
  )


#removing self-employed
data <- data %>%
  # A. Remove Self-Employed and Unpaid Workers
  filter(!str_detect(tolower(as.character(classwkr)), "self-employed|unpaid family worker")) %>%
  
  # B. Remove Zero/Negative/Missing Earners
mutate(
  log_weekly_wage = log(weekly_wage)
) %>%
  filter(!is.na(log_weekly_wage), log_weekly_wage > -Inf)
   
print(paste("Final Replication Sample Size:", nrow(data)))

# Define the internal integer indices that correspond to your desired IPUMS codes
PRIVATE_INDEX <- 8  # Corresponds to IPUMS code 21
PUBLIC_INDICES <- c(1, 2, 3, 6) # Corresponds to IPUMS codes 26, 28, 27, 25

data <- data %>%
  # A. FILTER: Remove Non-Wage/Salary and Invalid Wages
  # Filter rows based on the internal index values (4, 5, 6, 7, 8 in the old Tibble view)
  filter(as.numeric(classwkr) %in% c(PRIVATE_INDEX, PUBLIC_INDICES)) %>%
  filter(weekly_wage > 0) %>%
  
  # B. MUTATE: Create Log Wage
  mutate(log_weekly_wage = log(weekly_wage)) %>%
  
  # C. MUTATE: Create Public/Private Sector (CLASS_SECTOR)
  mutate(
    # Get the internal index for the case_when check
    classwkr_index = as.numeric(classwkr),
    
    class_sector = case_when(
      classwkr_index == PRIVATE_INDEX ~ "Private (Combined)",
      classwkr_index %in% PUBLIC_INDICES ~ "Public"
      # NA values are handled by the filter above
    ),
    class_sector = factor(class_sector)
  )
  
  # D. MUTATE: Create Industry Sector (CARE_OTHER) - CORRECTED
CARE_CODES_APPENDIX <- c(7860, 7870, 7880, 7890, 7970, 7980, 7990, 8070, 8080, 
                         8090, 8170, 8180, 8190, 8270, 8290, 8370, 8380, 8390, 8470)
data <- data %>%
  mutate(
    # Use the full list of codes from the appendix
    industry_sector = case_when(
      ind %in% CARE_CODES_APPENDIX ~ "Care Industries", 
      TRUE ~ "Other Industry"
    ),
    industry_sector = factor(industry_sector)
  )

# E. MUTATE: Create Sex/Gender Category
data <- data %>%
  mutate(
    sex_index = as.numeric(sex),
    sex_category = case_when(
      sex_index == 1 ~ "Women", 
      sex_index == 2 ~ "Men", 
      TRUE ~ "Other/NA"
    ),
    sex_category = factor(sex_category))

print(paste("Final Replication Sample Size:", nrow(data)))  

#Replicating Table 1
# Assuming the data object has been correctly created and filtered.

# A. Function to Calculate Care/Other Row Percentages
calculate_row_percent <- function(data_frame, grouping_vars) {
  data_frame %>%
    group_by(across(all_of(grouping_vars))) %>%
    summarise(
      Care_Count = sum(industry_sector == "Care Industries"),
      Other_Count = sum(industry_sector == "Other Industry"),
      N = n()
    ) %>%
    ungroup() %>%
    mutate(
      Care_Percent = round(Care_Count / N * 100, 1), # Round to 1 decimal place
      Other_Percent = round(Other_Count / N * 100, 1)
    )
}

# B. Calculate all groups required for the table:

# 1. Overall Total Sample Row
total_sample_row <- calculate_row_percent(data, NULL) %>%
  mutate(Group = "Total", Subgroup = "Total")

# 2. Total by Sex Rows (Overall Women/Men totals)
sex_totals_rows <- calculate_row_percent(data, "sex_category") %>%
  rename(Subgroup = sex_category) %>%
  mutate(Group = "Total")

# 3. Class Sector Totals (Row totals for Private Combined / Public, across both sexes)
class_sector_totals <- calculate_row_percent(data, "class_sector") %>%
  rename(Group = class_sector) %>%
  mutate(Subgroup = "Total")

# 4. Detailed Class Sector by Sex Rows (The combined breakdown by sex)
class_sector_by_sex <- calculate_row_percent(data, c("class_sector", "sex_category")) %>%
  rename(Group = class_sector, Subgroup = sex_category)

# C. Combine Results and Display
final_table1_data <- bind_rows(
  total_sample_row,
  sex_totals_rows,
  class_sector_totals,
  class_sector_by_sex
) %>%
  select(Group, Subgroup, Care_Percent, Other_Percent, N)

print("Final Table 1 Replication Data (Care vs. Other Row %)")
print(final_table1_data, n = Inf)

#Edited formatting for  better tables
final_table_formatted <- final_table1_data %>%
  
  # A. Create a new, explicit 'Type' column for all rows (for sorting)
  mutate(
    Type = case_when(
      # Assign a numeric order to match the visual structure of Table 1
      Group == "Total" & Subgroup == "Total" ~ 1,
      Group == "Total" & Subgroup == "Women" ~ 2,
      Group == "Total" & Subgroup == "Men" ~ 3,
      
      # Private Section
      Group == "Private (Combined)" & Subgroup == "Total" ~ 4,
      Group == "Private (Combined)" & Subgroup == "Women" ~ 5,
      Group == "Private (Combined)" & Subgroup == "Men" ~ 6,
      
      # Public Section
      Group == "Public" & Subgroup == "Total" ~ 7,
      Group == "Public" & Subgroup == "Women" ~ 8,
      Group == "Public" & Subgroup == "Men" ~ 9
    )
  ) %>%
  
  # B. Filter down to the exact 9 rows we want to display (using the new Type column)
  filter(Type %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9)) %>%
  
  # C. Sort by the custom Type column
  arrange(Type) %>%
  
  # D. Create the final Row Label based on the formatting logic
  mutate(
    `Row Label` = case_when(
      # The main group labels (Total, Private, Public)
      Type == 1 ~ "Total",
      Type == 4 ~ "Private, for-profit", # Substitute label for the combined group
      Type == 7 ~ "Public",
      # The subgroup labels (Men, Women)
      TRUE ~ as.character(Subgroup)
    )
  ) %>%
  
  # E. Select and rename columns for final table headers, and REMOVE helper columns
  select(
    `Row Label`,
    `Care Industries` = Care_Percent,
    Other = Other_Percent
  )

# Final Printout
print("Final Table 1 Replication Data (Corrected Format)")

#Enhancing table1
final_output_table <- final_table_formatted %>%
  # Select only the columns needed for the visual table
  select(
    `Row Label`,
    `Care Industries`,
    Other
  )

# Use the kable function to render the table in Markdown/HTML format
# This will display the lines and boxes you are looking for.
kable(final_output_table, 
      format = "html", 
      caption = "Table 1: Type of Employer in Care and Other Industries (Replication)",
      align = 'lrrr') %>%
  kable_styling(full_width = FALSE, 
                bootstrap_options = c("striped", "hover", "condensed"))

# Replicating Table 2
# Calculate Wage Percentiles (Table 2 Data)
# A. CORRECTED Function to calculate the P10, P50, and P90 percentiles
calculate_percentiles <- function(data_frame, grouping_vars) {
  data_frame %>%
    group_by(across(all_of(grouping_vars))) %>%
    summarise(
      N = n(),
      P10 = quantile(weekly_wage * 52, probs = 0.10, na.rm = TRUE),
      `P50 (Median)` = quantile(weekly_wage * 52, probs = 0.50, na.rm = TRUE), # Column name fixed here
      P90 = quantile(weekly_wage * 52, probs = 0.90, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    
    # B. Calculate Earnings Ratios (using the correct P50 column name)
    mutate(
      P10_P50 = round((P10 / `P50 (Median)`) * 100, 1), 
      P50_P90 = round((`P50 (Median)` / P90) * 100, 1)
    )
}

# C. Calculate Percentiles for Industry Sectors (Total, Women, Men)
industry_totals <- calculate_percentiles(data, "industry_sector") %>%
  rename(Row_Label = industry_sector) %>%
  mutate(Sex_Group = "Total")

# D. Care/Other Industries by Sex
industry_by_sex <- calculate_percentiles(data, c("industry_sector", "sex_category")) %>%
  rename(Row_Label = industry_sector, Sex_Group = sex_category)


# E. Combine and Format Results
wage_percentile_data_full <- bind_rows(
  industry_totals,
  industry_by_sex
) %>%
  
  # Setting the final display order 
  mutate(
    Sort_Order = case_when(
      Row_Label == "Care Industries" & Sex_Group == "Total" ~ 1,
      Row_Label == "Care Industries" & Sex_Group == "Women" ~ 2,
      Row_Label == "Care Industries" & Sex_Group == "Men" ~ 3,
      Row_Label == "Other Industry" & Sex_Group == "Total" ~ 4,
      Row_Label == "Other Industry" & Sex_Group == "Women" ~ 5,
      Row_Label == "Other Industry" & Sex_Group == "Men" ~ 6
    )
  ) %>%
  arrange(Sort_Order) %>%
  
  # Clean up the Row Label column for presentation
  mutate(
    `Row Label` = case_when(
      Sex_Group != "Total" ~ as.character(Sex_Group),
      TRUE ~ as.character(Row_Label)
    )
  ) %>%
  
  # Round dollar values and select final columns for Table 2
  mutate(
    P10 = round(P10, 0),
    P90 = round(P90, 0)
  )
wage_percentile_data <- wage_percentile_data_full %>% 
  select(
    `Row Label`,
    P10,
    `P50 (Median)`,
    P90,
    `P10/P50` = P10_P50, 
    `P50/P90` = P50_P90
  ) %>%
  
  mutate(
    P10 = dollar(P10, accuracy = 1), # Adds $ and comma separators
    `P50 (Median)` = dollar(`P50 (Median)`, accuracy = 1),
    P90 = dollar(P90, accuracy = 1)
  )

# C.Saving the table 
kable(wage_percentile_data, 
      format = "html", # or "html" if running in RStudio/Jupyter
      caption = "Table 2: Annual Earnings at the 10th, 50th, and 90th Percentiles by Type of Industry (Replication)",
      align = 'lrrrrr') %>% # l = left align, r = right align
  kable_styling(full_width = FALSE, 
                              bootstrap_options = c("striped", "hover", "condensed"))
  

# Replicating table 3 

# 1. Creating Variables needed for Table-3
# race_eth is already a variable in the data

data_T3 <- data %>%
  
  # A. Defining education Levels
  mutate(
    educ_label = tolower(as.character(educ)),
    educ_level = case_when(
      str_detect(educ_label, "doctorate|professional school") ~ "Ph.D. or Professional degree",
      str_detect(educ_label, "master") ~ "Master's degree", 
      str_detect(educ_label, "bachelor|4 years of college") ~ "College graduate",
      str_detect(educ_label, "associate|some college|1 year of college|2 years of college|3 years of college") ~ "Some college",
      str_detect(educ_label, "high school diploma|12th grade, diploma") ~ "High school degree",
      str_detect(educ_label, "grade|no diploma|preschool|no schooling") & 
        !str_detect(educ_label, "high school diploma") ~ "Less than high school", 
      TRUE ~ "NA/Other"
    ),
    educ_level = factor(educ_level)
  ) %>%
  
  # B.Marital Status and Children 
  mutate(
    # Marital Status
    marst_label = tolower(as.character(marst)),
    is_married = as.integer(
      coalesce(
        str_detect(marst_label, "married, spouse present|married, spouse absent"),
        FALSE
      )
    ), 
    
    # Children Status
    nchild_label = as.character(nchild),
    has_children = as.integer(
      coalesce(
        (nchild_label != "0"), # TRUE if not '0'
        FALSE # Treat NA as FALSE (no children)
      )
    )
  ) %>%
  
  # C. Race/Ethnicity
  mutate(
    hisp_char = tolower(as.character(hisp_ch)),
    race_char = tolower(as.character(race)),
    
    race_ethnicity_recoded = case_when(
      # 1. Hispanic - If HISPAN is anything other than the "Not Hispanic" label)
      !str_detect(hisp_char, "not hispanic|blank|niu|not available|missing|unknown") ~ "Hispanic", 
      
      # Now non-Hispanic categories as hispanics already filtered out
      # 2. White, non-Hispanic
      str_detect(race_char, "white") ~ "White, non-Hispanic", 
      
      # 3. Black, non-Hispanic
      str_detect(race_char, "black|african") ~ "Black, non-Hispanic", 
      
      # 4. Other, non-Hispanic (Any other race label not caught above)
      TRUE ~ "Other, non-Hispanic"
    ),
    
    # Setting the order to match the required table structure
    race_ethnicity_recoded = factor(race_ethnicity_recoded,
                                    levels = c(
                                      "White, non-Hispanic",
                                      "Black, non-Hispanic",
                                      "Other, non-Hispanic",
                                      "Hispanic"
                                    )
    )
  )
# 2. Calculation of Table 3

grouping_vars <- c("industry_sector", "sex_category")

# A. Calculate Mean Age and Binary Variable Means like children, marriage,etc.
means_data <- data_T3 %>%
  group_by(across(all_of(grouping_vars))) %>%
  summarise(
    `Age (mean)` = round(mean(age), 1), 
    `Percent married` = round(mean(is_married) * 100, 1),
    `Percent with children` = round(mean(has_children) * 100, 1)
  ) %>%
  pivot_longer(cols = starts_with(c("Age", "Percent")), 
               names_to = "Characteristic", 
               values_to = "Value")

# B. Calculate Education Percentages
education_data <- data_T3 %>%
  group_by(across(all_of(grouping_vars)), educ_level) %>%
  summarise(N = n()) %>%
  ungroup() %>%
  group_by(across(all_of(grouping_vars))) %>%
  mutate(Value = round(N / sum(N) * 100, 1)) %>%
  filter(educ_level != "NA/Other") %>% 
  select(Characteristic = educ_level, Value)

# C. Calculate Race/Ethnicity Percentages
race_data <- data_T3 %>%
  group_by(across(all_of(grouping_vars)), race_ethnicity_recoded) %>%
  summarise(N = n()) %>%
  ungroup() %>%
  group_by(across(all_of(grouping_vars))) %>%
  mutate(Value = round(N / sum(N) * 100, 1)) %>%
  # No filtering needed here if RACE_ETHN is perfect, but kept for robustness 
  filter(!is.na(race_ethnicity_recoded)) %>% 
  select(Characteristic = race_ethnicity_recoded, Value)


# D. Combine All Data and Pivot Wider to match Table 3 format
final_table_3_data_raw <- bind_rows(means_data, education_data, race_data)

final_table_3_wide <- final_table_3_data_raw %>%
  mutate(
    Header = paste(industry_sector, sex_category)
  ) %>%
  pivot_wider(
    id_cols = Characteristic, 
    names_from = Header, 
    values_from = Value
  ) %>%
  
  # E. Sorting the rows to match the order in the Table 3
  mutate(
    Sort_Order = case_when(
      Characteristic == "Age (mean)" ~ 1,
      Characteristic == "Less than high school" ~ 3,
      Characteristic == "High school degree" ~ 4,
      Characteristic == "Some college" ~ 5,
      Characteristic == "College graduate" ~ 6,
      Characteristic == "Master's degree" ~ 7,
      Characteristic == "Ph.D. or Professional degree" ~ 8,
      Characteristic == "Percent married" ~ 10,
      Characteristic == "Percent with children" ~ 11,
      Characteristic == "White, non-Hispanic" ~ 13,
      Characteristic == "Black, non-Hispanic" ~ 14,
      Characteristic == "Other, non-Hispanic" ~ 15,
      Characteristic == "Hispanic" ~ 16,
      TRUE ~ 99
    )
  ) %>%
  filter(Sort_Order < 99) %>% 
  arrange(Sort_Order) %>%
  select(
    Characteristic,
    `Care Industries Women`,
    `Care Industries Men`,
    `Other Industry Women`,
    `Other Industry Men`
  )


print("--- Table 3 Replication Data (FINAL SCRIPT) ---")
print(final_table_3_wide, n = Inf)

# Final Table 3
library(kableExtra)
kable(final_table_3_wide,
      format = "html",
      caption = "Table 3: Characteristics of Workers by Type of Industry and Gender (Replication)",
      align = 'lrrrr') %>%
  kable_styling(full_width = FALSE, 
                              bootstrap_options = c("striped", "hover", "condensed"))

#TABLE 5 REPLICATION

# Ensuring data exists
if (!exists("data")) stop("Object `data` not found. Run your preprocessing first (Tables 1-3 pipeline).")

#creating Occupational category variable

data <- data %>%
  mutate(
    occ_ch = as.character(occ),
    # remove leading zeros if present, but keep numeric semantics
    occ_ch_clean = ifelse(!is.na(occ_ch) & str_detect(occ_ch, "^0+[0-9]+$"),
                          str_replace(occ_ch, "^0+", ""),
                          occ_ch),
    occ_num = suppressWarnings(as.numeric(occ_ch_clean))
  ) %>%

# 2) Create occupational_category using the ranges you used in 2015 paper and from 2011-19 
#    Managers (0010 - 0970), Professionals (1000 - 3540), else Other occupations
  mutate(
    occupational_category = case_when(
      !is.na(occ_num) & occ_num >= 10  & occ_num <= 970  ~ "Managers",
      !is.na(occ_num) & occ_num >= 1000 & occ_num <= 3540 ~ "Professionals",
      is.na(occ_num) & str_detect(tolower(occ_ch), "manager|management|mgr") ~ "Managers",
      is.na(occ_num) & str_detect(tolower(occ_ch), "professional|professor|engineer|analyst|scientist|therapist|nurse|doctor") ~ "Professionals",
      TRUE ~ "Other occupations"
    ),
    occupational_category = factor(occupational_category,
                                   levels = c("Managers", "Professionals", "Other occupations"))
  ) %>%

# 3) Creating occupation dummies used by the regression
  mutate(
    occ_man = as.integer(occupational_category == "Managers"),
    occ_prof = as.integer(occupational_category == "Professionals")
  )


# 1. Prepare variables for regression
data_T5 <- data %>%
  # numeric conversions for being on safer side
  mutate(
    incwage_num = suppressWarnings(as.numeric(as.character(incwage))),
    wkswork_num = suppressWarnings(as.numeric(as.character(wkswork2))),
    hrswork_num = suppressWarnings(as.numeric(as.character(uhrsworkt)))
  ) %>%
  
  # computing hourly earnings: incwage is annual; weekly midpoint used earlier as weeks_worked_mid:
  # we can use existening one also 
  mutate(
    wkswork2_ch = as.character(wkswork2),
    wks_num = suppressWarnings(as.numeric(wkswork2_ch)),
    weeks_worked_mid = case_when(
      !is.na(wks_num) & wks_num == 1 ~ 7,
      !is.na(wks_num) & wks_num == 2 ~ 20,
      !is.na(wks_num) & wks_num == 3 ~ 33,
      !is.na(wks_num) & wks_num == 4 ~ 43.5,
      !is.na(wks_num) & wks_num == 5 ~ 48.5,
      !is.na(wks_num) & wks_num == 6 ~ 51,
      str_detect(tolower(wkswork2_ch), "1-13|1 to 13") ~ 7,
      str_detect(tolower(wkswork2_ch), "14-26|14 to 26") ~ 20,
      str_detect(tolower(wkswork2_ch), "27-39|27 to 39") ~ 33,
      str_detect(tolower(wkswork2_ch), "40-47|40 to 47") ~ 43.5,
      str_detect(tolower(wkswork2_ch), "48-50|48 to 50") ~ 48.5,
      str_detect(tolower(wkswork2_ch), "51-52|51 to 52") ~ 51,
      TRUE ~ NA_real_
    ),
    
    # hourly earnings (annual / (weeks * hours_per_week))
    hourly_earnings = case_when(
      !is.na(incwage_num) & !is.na(weeks_worked_mid) & !is.na(hrswork_num) &
        weeks_worked_mid > 0 & hrswork_num > 0 ~ incwage_num / (weeks_worked_mid * hrswork_num),
      TRUE ~ NA_real_
    ),
    
    ln_ahe = ifelse(!is.na(hourly_earnings) & hourly_earnings > 0, log(hourly_earnings), NA_real_)
  ) %>%
  
  # drop unusable rows
  filter(!is.na(ln_ahe), !is.na(industry_sector), !is.na(occupational_category), !is.na(classwkr))

# 2. Create job-characteristic dummies used in the table

data_T5 <- data_T5 %>%
  
  # industry: Care Industries vs Other Industry
  mutate(
    care_ind = as.integer(industry_sector == "Care Industries")
  ) %>%
  
  # occupation dummies (Managers ; Professionals ; Other occupations is omitted base)
  mutate(
    occ_man = as.integer(occupational_category == "Managers"),
    occ_prof = as.integer(occupational_category == "Professionals")
  ) %>%
  
  # classwkr -> distinguish private for-profit, private non-profit(not present in our data, so omitting)
  mutate(
    classwkr_ch = tolower(as.character(classwkr)),
    # All private wage/salary workers = private for-profit
    private_for_profit = as.integer(str_detect(classwkr_ch, "private")),
    # No non-profit workers in your dataset → set to 0 always
    private_nonprofit = 0,
    # Government employees (local, state, federal)
    public_sector = as.integer(str_detect(classwkr_ch,
                                          "local government|state government|federal government"))
  )

# 3. Worker characteristics: sex (Female), age, education dummies, marital, children, race dummies

# Education
if (!"educ_level" %in% names(data_T5)) {
  data_T5 <- data_T5 %>%
    mutate(educ_label = tolower(as.character(educ)),
           educ_level = case_when(
             str_detect(educ_label, "doctorate|professional") ~ "Ph.D./Professional",
             str_detect(educ_label, "master") ~ "Master's",
             str_detect(educ_label, "bachelor|4 years of college") ~ "College degree",
             str_detect(educ_label, "associate|some college|1 year of college|2 years of college|3 years of college") ~ "Some college",
             str_detect(educ_label, "high school diploma|12th grade, diploma") ~ "High school degree",
             TRUE ~ "Less than high school"
           ))
}

data_T5 <- data_T5 %>%
  mutate(
    female = as.integer(tolower(as.character(sex)) %in% c("female", "f", "women")),
    age = suppressWarnings(as.numeric(as.character(age))),
    educ_less_hs = as.integer(educ_level == "Less than high school"),
    educ_hs = as.integer(educ_level == "High school degree"),
    educ_somecollege = as.integer(educ_level == "Some college"),
    educ_college = as.integer(educ_level == "College degree"),
    educ_masters = as.integer(educ_level == "Master's"),
    educ_phd = as.integer(educ_level %in% c("Ph.D./Professional", "Ph.D. or Professional degree")),
    is_married = as.integer(tolower(as.character(marst)) %in% c("married, spouse present","married, spouse absent")),
    has_children = as.integer(!is.na(nchild) & suppressWarnings(as.numeric(as.character(nchild))) > 0)
  )

# Race/ethnicity: use race_eth 
if ("race_eth" %in% names(data_T5)) {
  data_T5 <- data_T5 %>%
    mutate(race_eth2 = as.character(race_eth))
} else if ("race_ethnicity_recoded" %in% names(data_T5)) {
  data_T5 <- data_T5 %>% mutate(race_eth2 = as.character(race_ethnicity_recoded))
} else {
  data_T5 <- data_T5 %>%
    mutate(race_ch = tolower(as.character(race)),
           hisp_char = tolower(as.character(hisp_ch)),
           race_eth2 = case_when(
             !is.na(hisp_char) & str_detect(hisp_char, "hisp") & !str_detect(hisp_char, "not hispanic") ~ "Hispanic",
             str_detect(race_ch, "white") ~ "White, non-Hispanic",
             str_detect(race_ch, "black|african") ~ "Black, non-Hispanic",
             TRUE ~ "Other, non-Hispanic"
           ))
}

# making dummies (like White, non-Hispanic)
data_T5 <- data_T5 %>%
  mutate(
    black_non_hisp = as.integer(race_eth2 == "Black, non-Hispanic"),
    other_non_hisp = as.integer(race_eth2 == "Other, non-Hispanic"),
    hispanic_flag = as.integer(race_eth2 == "Hispanic")
  )

# 4. Set base categories by dropping one category per group (we will omit explicit intercept dummies in model formula)
# We can include education dummies for HS, some college, college, masters, phd; base = less than high school

# 5. Estimate models with robust SEs using estimatr::lm_robust
# Model I: Job characteristics only
m1 <- lm_robust(
  ln_ahe ~ care_ind + occ_man + occ_prof + private_for_profit,
  data = data_T5,
  se_type = "HC1"
)

# Model II: Job + Worker characteristics
m2 <- lm_robust(ln_ahe ~ care_ind + occ_man + occ_prof + private_for_profit +
                  female + age +
                  educ_hs + educ_somecollege + educ_college + educ_masters + educ_phd +
                  is_married + has_children +
                  black_non_hisp + other_non_hisp + hispanic_flag,
                data = data_T5, se_type = "HC1")

# Model III: Add class sector (public vs private) explicitly (public is omitted base)
m3 <- lm_robust(ln_ahe ~ care_ind + occ_man + occ_prof + private_for_profit +
                  female + age +
                  educ_hs + educ_somecollege + educ_college + educ_masters + educ_phd +
                  is_married + has_children +
                  black_non_hisp + other_non_hisp + hispanic_flag +
                  public_sector, # add as additional job/class control
                data = data_T5, se_type = "HC1")

# 6. Prepare coefficient name mapping (friendly names like in your Table 5)
coef_map <- c(
  "care_ind" = "Care work industry",
  "occ_man" = "Manager",
  "occ_prof" = "Professional",
  "private_for_profit" = "Private, for-profit",
  "private_nonprofit" = "Private, non-profit",
  "female" = "Female",
  "age" = "Age",
  "educ_hs" = "High school degree",
  "educ_somecollege" = "Some college",
  "educ_college" = "College degree",
  "educ_masters" = "Master's degree",
  "educ_phd" = "Ph.D./ Professional degree",
  "is_married" = "Married",
  "has_children" = "Children",
  "black_non_hisp" = "Black, not Hispanic",
  "other_non_hisp" = "Other, not Hispanic",
  "hispanic_flag" = "Hispanic",
  "public_sector" = "Public sector"
)

# 7. Model table with modelsummary
models_list <- list("Model I" = m1, "Model II" = m2, "Model III" = m3)

# Options for modelsummary
msummary_opts <- list(
  coef_map = coef_map,
  gof_map = c("r.squared","nobs"),            # include R-squared and N
  stars = c(0.001, 0.01, 0.05)                  # ** p<0.01, * p<0.05, *** p<0.001
)

# Print table in console (html)
modelsummary(models_list,
             coef_map = coef_map,
             stars = c("***" = 0.001, "**" = 0.01, "*" = 0.05),
             fmt = 2,
             gof_map = c("r.squared","nobs"),
             output = "table5.html")

# End of replication exercise: i have replicated 4 tables; 1,2,3,5(econometric model)

