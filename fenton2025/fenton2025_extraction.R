# Extract LMS parameters from publicly available Excel spreadsheet
#
# "Fenton Third-Generation Growth Charts of Preterm Infants Without Abnormal Fetal Growth: A Systematic Review and Meta-Analysis"
# - Fenton TR, Elmrayed S, Alshaikh BN. Paediatr Perinat Epidemiol. 2025 Jun 19. PMID: 40534585  DOI: 10.1111/ppe.70035
# - https://pubmed.ncbi.nlm.nih.gov/40534585/
#
# "Actual age calculator", from: https://ucalgary.ca/resource/preterm-growth-chart/calculators-apps
# - 2025-06-28: https://ucalgary.ca/live-uc-ucalgary-site/sites/default/files/teams/418/2025%20Fenton%20Growth%20Chart%20Clinical%20Calculator%20v1.1.xlsx
#
# To obtain LMS data
# - open Excel spreadsheet in Google Sheets
# - or run following R code, to avoid needing to copy-paste

library(tidyverse)
library(readxl)
library(here)

extract_data <- function(sex) {
  filepath <- here("fenton2025", "2025 Fenton Growth Chart Clinical Calculator v1.1.xlsx")
  df_sheet <- suppressMessages(read_excel(filepath, sheet = 'Calculator for exact age'))
  
  if (sex == 'm') {
    lmsdata <- df_sheet[c(11:204), c(35:38, 45:48, 55:58)] # Boy Wt / Len / Head
  } else {
    lmsdata <- df_sheet[c(11:204), c(65:68, 75:78, 85:88)] # Girl Wt / Len / Head
  }
  lmsdata <- lmsdata |> 
    mutate_all(list(as.numeric)) |> 
    set_names(
      c(
        'weight_age', 'weight_L', 'weight_M', 'weight_S',
        'length_age', 'length_L', 'length_M', 'length_S',
        'head_age', 'head_L', 'head_M', 'head_S'
      )
    ) |> 
    pivot_longer(
      # ".value" indicates that the corresponding component of the column name defines the
      # name of the output column containing the cell values, overriding values_to entirely
      cols = everything(),
      names_to = c("measure", ".value"),
      names_sep = "_"
    ) |> 
    filter(!is.na(age)) |> 
    transmute( # add metadata
      chart = "fenton_2025",
      age = 22 + age/7, # convert days after 22 weeks to weeks PMA
      age_units = "weeks",
      gender = sex,
      measure = ifelse(measure == "head", "head_circ", measure),
      measure_units = ifelse(measure == "weight", "g", "cm"),
      L, M, S
    ) |> 
    arrange(measure, age) |> 
    as.data.frame()
}

##### Generate LMS dataframe #####
lmsdata <- bind_rows(extract_data('m'), extract_data('f')) |> 
  arrange(gender, measure, age)

write.csv(lmsdata, file = here("fenton2025", "lmsdata.csv"), row.names = FALSE)


##### Tests #####

lmsdata |> group_by(measure, gender) |> summarize(min(age), max(age), n())

# Plot median weight for both genders
lmsdata %>%
  filter(measure == 'weight') %>%
  ggplot(aes(age, M, color = gender)) + geom_line()

# Plot median length and head circumference for both genders
lmsdata %>%
  filter(measure != 'weight') %>%
  ggplot(aes(age, M, color = measure)) + geom_line(aes(linetype = gender))

