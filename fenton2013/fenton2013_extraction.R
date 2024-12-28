# Extract LMS parameters from publicly available Excel spreadsheet
#
# "Actual age calculator", from: https://ucalgary.ca/resource/preterm-growth-chart/calculators-apps
# - 2024-12-24: https://ucalgary.ca/live-uc-ucalgary-site/sites/default/files/teams/418/clinical-exact-age-calculator-fenton-2013-growth-chart-v7.xlsx
#
# To obtain LMS data
# - open Excel spreadsheet in Google Sheets
# - or run following R code, to avoid needing to copy-paste

library(tidyverse)
library(readxl)
library(here)

##### Function to extract and format LMS data from Excel file

extract_data <- function(sheet) {
  filepath <- here("fenton2013", "clinical-exact-age-calculator-fenton-2013-growth-chart-v7.xlsx")
  sex <- ifelse(sheet == "Girls", "f", "m")
  
  df_sheet <- suppressMessages(read_excel(filepath, sheet = sheet))
  
  lmsdata <- df_sheet[c(6:199), c(30:33, 40:43, 50:53)] %>% # weight, length, head
    mutate_all(list(as.numeric)) %>%
    set_names(
      c(
        'weight_age', 'weight_L', 'weight_M', 'weight_S',
        'length_age', 'length_L', 'length_M', 'length_S',
        'head_age', 'head_L', 'head_M', 'head_S'
      )    
    ) %>% 
    pivot_longer(
      # ".value" indicates that the corresponding component of the column name defines the
      # name of the output column containing the cell values, overriding values_to entirely
      cols = everything(),
      names_to = c("measure", ".value"),
      names_sep = "_"
    ) %>% 
    filter(!is.na(age)) %>% 
    transmute( # add metadata
      chart = "fenton_2013",
      age = 22 + age/7, # convert days after 22 weeks to weeks PMA
      age_units = "weeks",
      gender = sex,
      measure = ifelse(measure == "head", "head_circ", measure),
      measure_units = ifelse(measure == "weight", "g", "cm"),
      L, M, S
    ) %>% 
    as.data.frame()
}

##### Generate LMS dataframe #####

lmsdata <- bind_rows(
  extract_data("Boys"),
  extract_data("Girls")) %>%
  arrange(gender, measure, age)

write.csv(lmsdata, file = here("fenton2013", "lmsdata.csv"), row.names = FALSE)

##### Tests #####
lmsdata %>% count(measure, gender)

# Plot median weight for both genders
lmsdata %>%
  filter(measure == 'weight') %>%
  ggplot(aes(age, M, color = gender)) + geom_line()

# Plot median length and head circumference for both genders
lmsdata %>%
  filter(measure != 'weight') %>%
  ggplot(aes(age, M, color = measure)) + geom_line(aes(linetype = gender))

