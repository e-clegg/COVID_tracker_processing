
# Script to process the WHO Clinical Trial dataset for Tableau

# 1: Load packages----
library(googlesheets4)
library(gargle)
library(dplyr)
library(tidyr)
library(scales)
library(readxl)
library(writexl)
library(stringr)
library(countrycode)

# Google Sheets connection ------------------------------------------------

# Authorise googlesheets4 to view and manage sheets on Drive
gs4_auth(email = "e.clegg@ukcdr.org.uk",
         token = "token.rds")

# Read data ---------------------------------------------------------------

# URL of Covid Tracker google sheet (WHO ICTRP Dataset tab)
# covid_tracker_url <- "https://docs.google.com/spreadsheets/d/1zMGzEJqAYHT4k1HkbmQDPvaaINrBgFRq1X5G6QptiKE/edit#gid=2099596279"
# 
# covid_gs <- as_sheets_id(covid_tracker_url)
# 
# who_trials_data <- read_sheet(covid_gs,
#                          sheet = "WHO ICTRP Dataset ",
#                          n_max = 5000,
#                          skip = 1) %>% 
#                          select(-1) # remove commented first column

# Read HG cleaned data in from Excel
who_trials_data <- read_xlsx("WHO Clinical Trials/COVID19-trials WHO_26 June 20_cleaned.xlsx")

# Separate entries in Country field (over multiple rows), and remove white space from field
trial_data_split_country <- who_trials_data %>%
  separate_rows(`Countries cleaned`, sep = ";", convert = FALSE) %>%
  mutate(Country = str_trim(`Countries cleaned`)) %>% 
  mutate(Country = str_replace_all(Country, c("England|Scotland|UK|UNITED KINGDOM"), "United Kingdom"),
         Country = str_replace_all(Country, c("USA|UNITED STATES|United states|United States of America|U.S."), "United States"),
         Country = str_replace_all(Country, "The Netherlands", "Netherlands"),
         Country = str_replace_all(Country, "The Philippines", "Philippines"),
         Country = str_replace_all(Country, "Viet Nam", "Vietnam")) %>% 
  unique()


# Read in DAC country lookup
dac_lookup <- read_xlsx("Lookups/Country lookup - Tableau and DAC Income Group.xlsx")

# Check countries that are unmatched (this information will be lost)
unmatched_countries <- trial_data_split_country %>%
                          filter(!(Country %in% dac_lookup$country_name)) %>% 
                          select(Country) %>% 
                          unique()

# Extract continent information from Country field
trial_data_split_country_2 <- trial_data_split_country %>%
      mutate(Continent = if_else(str_to_upper(Country) %in% c("AFRICA", "ASIA", "EUROPE", "NORTH AMERICA", "SOUTH AMERICA", "OCEANIA"), Country, ""))

# Remove countries not recognised by Tableau
trial_data_split_country_3 <- trial_data_split_country_2 %>% 
      mutate(Country = if_else(Country %in% dac_lookup$country_name, Country, "Unknown"))

# Match continent to country names
trial_data_split_country_4 <- trial_data_split_country_3 %>% 
     mutate(Continent = coalesce(countrycode(sourcevar = Country,
                                             origin = "country.name",
                                             destination = "continent"), Continent))

# Manually edit North and South America (these are grouped by default)
trial_data_split_country_5 <- trial_data_split_country_4 %>% 
    mutate(Continent = if_else(Country %in% c("Brazil", "Argentina", "Colombia", "Peru", 
                                              "Chile", "Ecuador", "Venezula", "Bolivia",
                                              "Uruguay", "Guyana", "Paraguay", "Peru",
                                              "Suriname"), "South America",
                               if_else(Continent == "Americas", "North America",
                                       Continent))) %>% 
    mutate(Continent = coalesce(Continent, "Unknown"))


# Add DAC income group
trial_data_dac_list_6 <- trial_data_split_country_5 %>% 
      left_join(dac_lookup, by = c("Country" = "country_name")) %>% 
      mutate(`DAC Income Group` = if_else(Country == "Unknown", "Unknown",
                                          if_else(Country == "China", "China",
                                                  coalesce(`DAC Income Group`, "Non-DAC list")))) %>% 
      mutate(`DAC Income Group` = if_else(`DAC Income Group` == "UMICs", "UMICs (without China)", `DAC Income Group`))


# Tidy inclusion/exclusion criteria fields
who_trial_data_final <- trial_data_dac_list_6 %>% 
        mutate(`Inclusion Criteria` = str_replace_all(`Inclusion Criteria`, c("Inclusion criteria:|Inclusion Criteria:|<br>|â€¢"), ""),
               `Exclusion Criteria` = str_replace_all(`Exclusion Criteria`, c("Exclusion criteria:|Exclusion Criteria:|<br>|â€¢"), ""),
               `Inclusion Criteria` = str_trim(str_replace_all(`Inclusion Criteria`, "\t", " ")),
               `Exclusion Criteria` = str_trim(str_replace_all(`Exclusion Criteria`, "\t", " "))) 


# Combine public, scientific abstract and inclusion criteria fields for searching
who_trial_data_final <- who_trial_data_final %>% 
        mutate(search_text = paste0(`Public title`, ";", `Scientific title`, ";", `Inclusion Criteria`))


# Write data  -----

# 1) to Excel
write_xlsx(who_trial_data_final, "WHO Clinical Trials/who_trial_data_final.xlsx")

# 2) to EC Google Drive (for Tableau)
results_url <- "https://docs.google.com/spreadsheets/d/1LN9EeJOFQP9sQVCsgoXomhY-LW8Gs6tz7QPKwt5xnz8/edit#gid=1621339980"
results <- as_sheets_id(results_url)

results_sheet <- sheet_write(who_trial_data_final,
                             ss = results,
                             sheet = "who_trials_data")


test <- filter(who_trial_data_final, Country == "Curacao")
