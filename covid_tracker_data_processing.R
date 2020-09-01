####
# UKCDR COVID-19 research project tracker - data processing script
#
# This script reads data from the "Funded Research Projects" tab of the 
# COVID-19 Tracker Google Sheet, reformats it and saves to a new 
# Google Sheet to feed into several Tableau visualisations
#
####

# Check required packages are installed 

if (!("devtools" %in% installed.packages())) {
  install.packages("devtools")
}
if (!("googlesheets4" %in% installed.packages())) {
  install.packages("googlesheets4")
}
if (!("gargle" %in% installed.packages())) {
  install.packages("gargle")
}
if (!("tidyverse" %in% installed.packages())) {
  install.packages("tidyverse")
}
if (!("scales" %in% installed.packages())) {
  install.packages("scales")
}
if (!("readxl" %in% installed.packages())) {
  install.packages("readxl")
}
if (!("stringr" %in% installed.packages())) {
  install.packages("stringr")
}
if (!("countrycode" %in% installed.packages())) {
  install.packages("countrycode")
}

# Load packages -----
library(googlesheets4)
library(gargle)
library(tidyverse)
library(scales)
library(readxl)
library(stringr)
library(countrycode)

# Read Tracker data from Google Sheet on UKCDR website ----

# Authentication
# token <- gs4_auth()
# saveRDS(token, "token.rds")

# Authorise googlesheets4 to view and manage sheets on EC Drive
# (using saved authentication token in folder)
gs4_auth(email = "e.clegg@ukcdr.org.uk",
         token = "token.rds")

# URL of Covid Tracker google sheet
covid_tracker_url <- "https://docs.google.com/spreadsheets/d/1zMGzEJqAYHT4k1HkbmQDPvaaINrBgFRq1X5G6QptiKE/edit#gid=2099596279"

covid_gs <- as_sheets_id(covid_tracker_url)

covid_data <- read_sheet(covid_gs,
                         range = "A:AZ",
                         sheet = "Funded Research Projects",
                         col_types = "c", # import columns as character
                         n_max = 10000) # set max number of rows (for quicker read-in)

# Format data for Tableau -------

# Clean data fields
covid_data_clean <- covid_data %>% 
        filter(!is.na(`Unique database reference number`)) %>% 
        mutate(
          `Primary WHO area` = str_replace_all(`PRIMARY WHO Research Priority Area Number`, ",", " "),
          `Primary WHO sub-area` = str_replace_all(`PRIMARY WHO Research Sub-Priority Number(s)`, ",", " "),
          `Secondary WHO area` = str_replace_all(`SECONDARY WHO Research Priority Area Number`, ",", " "),
          `Secondary WHO sub-area` = str_replace_all(`SECONDARY WHO Research Sub-Priority Number(s)`, ",", " "),  
          
          `Primary WHO area` = str_replace_all(`PRIMARY WHO Research Priority Area Number`, "[^[:alnum:].]+", ", "),
          `Primary WHO sub-area` = str_replace_all(`PRIMARY WHO Research Sub-Priority Number(s)`, "[^[:alnum:].]+", ", "),
          `Secondary WHO area` = str_replace_all(`SECONDARY WHO Research Priority Area Number`, "[^[:alnum:].]+", ", "),
          `Secondary WHO sub-area` = str_replace_all(`SECONDARY WHO Research Sub-Priority Number(s)`, "[^[:alnum:].]+", ", "),               
          `Funder(s)` = str_to_upper(coalesce(`Funder(s)`, "Unknown")),
          `Country` = if_else(is.na(`Country/ countries research is being are conducted`) | `Country/ countries research is being are conducted` == "unknown",
                              "Unknown", `Country/ countries research is being are conducted`),
          `Location of studies/trials` = coalesce(`Additional detail on location where research is being conducted`, `Country/ countries research is being are conducted`, "")
        )  

# Convert amount field to numeric
covid_data_clean <- covid_data_clean %>% 
                      mutate(new_string = substr(`Amount Awarded converted to USD`, 2, nchar(`Amount Awarded converted to USD`))) %>% 
                      mutate(new_string = str_replace_all(new_string, ",", "")) %>% 
                      mutate(`Amount ($)` = round(as.numeric(new_string)), 0) %>% 
                      select(-new_string)

        
# VISUALISATION 1 - TABLEAU WORLD MAP ------------------------------------

# Combine primary and secondary priority areas (these will both be
# searched by the Tableau map filters)

covid_data_country_clean <- covid_data_clean %>% 
  mutate(
    WHO_area = if_else(!is.na(`SECONDARY WHO Research Priority Area Number`), 
                       paste0(str_squish(`PRIMARY WHO Research Priority Area Number`), ", ", coalesce(str_squish(`SECONDARY WHO Research Priority Area Number`), "")),
                       str_squish(`PRIMARY WHO Research Priority Area Number`)),
    WHO_sub_area = if_else(!is.na(`SECONDARY WHO Research Sub-Priority Number(s)`),
                           paste0(coalesce(str_squish(`PRIMARY WHO Research Sub-Priority Number(s)`), ""), ", ", coalesce(str_squish(`SECONDARY WHO Research Sub-Priority Number(s)`), "")),
                           coalesce(str_squish(`PRIMARY WHO Research Sub-Priority Number(s)`), ""))
  ) %>%  
  select(-`PRIMARY WHO Research Priority Area Number`,
         -`SECONDARY WHO Research Priority Area Number`,
         -`PRIMARY WHO Research Sub-Priority Number(s)`,
         -`SECONDARY WHO Research Sub-Priority Number(s)`
  )


# Split project data by country -------

# Split country field across multiple rows (for plotting on a map
# in Tableau)

covid_data_split_country <- covid_data_country_clean %>%
                                  mutate(Country = str_replace_all(Country, " and ", ", ")) %>% 
                                  separate_rows(Country, sep = ",", convert = FALSE) %>%
                                  mutate(Country = str_trim(Country)) %>% 
                                  mutate(Country = str_replace_all(Country, c("UK|Scotland|Wales"), "United Kingdom"),
                                         Country = str_replace_all(Country, c("USA|UNITED STATES|United states"), "United States"),
                                         Country = str_replace(Country, "N/A", "Unknown"),
                                         Country = str_replace(Country, "The Netherlands", "Netherlands"),
                                         Country = str_replace(Country, "The Philippines", "Philippines"),
                                         Country = if_else(str_detect(Country, "Ivoire"), "Ivory Coast", Country),
                                         Country = str_replace(Country, "Republic of Congo", "Congo Republic"),
                                         Country = str_replace(Country, "DRC", "Democratic Republic of the Congo"),
                                         Country = str_replace_all(Country, "é", "e")) 
          
# Read in DAC country lookup and Tableau accepted country list
dac_lookup <- read_xlsx("Lookups/Country lookup - Tableau and DAC Income Group.xlsx")

# Check countries that are unmatched (this information will be lost)
unmatched_countries <- covid_data_split_country %>%
                            filter(!(Country %in% dac_lookup$country_name)) %>% 
                            select(Country) %>% 
                            unique()

# Replace country with "Unknown" if not recognised against Tableau's 
# accepted list
covid_data_split_country <- covid_data_split_country %>%
                              mutate(Country = if_else(Country %in% dac_lookup$country_name, Country, "Unknown")) %>% 
                              unique()

# Add continent to data, matching on Country
covid_data_split_country$Continent <- countrycode(sourcevar = covid_data_split_country[["Country"]],
                                                  origin = "country.name",
                                                  destination = "continent")

# Manually split up North and South America
covid_data_split_country <- covid_data_split_country %>% 
                               mutate(Continent = if_else(Country %in% c("Brazil", "Argentina", "Colombia", "Peru", 
                                                             "Chile", "Ecuador", "Venezula", "Bolivia",
                                                             "Uruguay", "Guyana", "Paraguay", "Peru",
                                                             "Suriname"), "South America",
                                                          if_else(Continent == "Americas", "North America",
                                                                  Continent))) %>% 
                               mutate(Continent = coalesce(Continent, "Unknown"))
                                            
# Add DAC income group to data, matching on country
covid_data_dac_list <- covid_data_split_country %>% 
                            left_join(dac_lookup, by = c("Country" = "country_name"))


# Write data to Google Sheet -----

# Results sheet stored on EC google drive
results_url <- "https://docs.google.com/spreadsheets/d/1LN9EeJOFQP9sQVCsgoXomhY-LW8Gs6tz7QPKwt5xnz8/edit#gid=1621339980"
results <- as_sheets_id(results_url)

results_sheet <- sheet_write(covid_data_dac_list,
                             ss = results,
                             sheet = "covid_data")



# VISUALISATION 2 - TABLEAU HEATMAP ----------------------------------------

# Input WHO priority area labels -----

who_area_names <- data.frame(
  number = as.character(c(1:9)),
  label = c("1. Virus: natural history, transmission and diagnostics", 
            "2. Animal and environmental research",
            "3. Epidemiological studies",
            "4. Clinical characterization and management",
            "5. Infection prevention and control",
            "6. Candidate therapeutics R&D",
            "7. Candidate vaccines R&D",
            "8. Ethics considerations for research",
            "9. Social sciences in the outbreak response"))

who_area_sub_names <- data.frame(
  number = c("1a", "1b", "1c", "1d", "1e", "1f",
             "2a", "2b", "2c",
             "3a", "3b", "3c", "3d",
             "4a", "4b", "4c", "4d", "4e", "4f",
             "5a", "5b", "5c", "5d",
             "6a", "6b", "6c", "6d", "6e", 
             "7a", "7b", "7c", "7d", "7e", 
             "8a", "8b", "8c", "8d", "8e", 
             "9a", "9b", "9c", "9d", "9e", "9f"),
  label = c("1a. Support development of diagnostic products to improve clinical processes", 
            "1b. Understand virus compartments, shedding and natural history of disease",
            "1c. Develop tools and conduct studies to monitor phenotypic change and potential adaptation",
            "1d. Characterize immunity (naturally acquired, population and vaccine-induced, including mucosal immunity)",
            "1e. Develop disease models (animal models and 3Rs approaches)",
            "1f. Virus stability in the environment",
            "2a. Investigation of animal source and route of transmission", 
            "2b. Socioeconomic and behavioural risk factors for spill-over",
            "2c. Risk reduction strategies at the human-animal environment interface",
            "3a. Transmission dynamics - clarify the relative importance of pre-symptomatic/asymptomatic transmission (including distinction between virus shedding and infectious transmission)", 
            "3b. Disease severity - Identify groups at high risk of severe infection; Determine the role of different age groups in transmission",
            "3c. Susceptibility - Determine if children are infected, and if so, are they infectious?",
            "3d. Control and mitigation measures - Predict the most effective measures to reduce the peak burden on healthcare providers and other societal functions; Estimate the effects of social distancing measures and other non-pharmaceutical interventions on transmissibility",
            "4a. Prognostic factors for severe disease (Different populations - pregnancy, young children, risk groups - immunosuppressed)", 
            "4b. Understand pathophysiology of COVID-19 infection, including understanding mild disease and the role of co-infections / infection, transmissibility, viral shedding",
            "4c. Optimal endpoints for clinical trials",
            "4d. Improve processes of care, including early diagnosis, discharge criteria; Determine interventions that improve the clinical outcome of infected patients (Steroids, High flow oxygen therapy)",
            "4e. Optimal adjuvant therapies for patients (and contacts)",
            "4f. Develop core clinical outcomes to maximize usability of data across range of trials",
            "5a. Effectiveness of restriction of movement of healthy exposed and infected persons to prevent secondary transmission (home, congregate setting, geographical restriction vs nothing)", 
            "5b. Effectiveness of specific PPE to reduce the risk of COVID-19 transmission among HCWs, patients and individuals in the community",
            "5c. Effectiveness of activities to minimize the role of the environment in COVID-19 transmission",
            "5d. Factors and methods influencing compliance with evidence-based IPC interventions during outbreak response",
            "6a. Develop in vitro and in vivo testing to identify candidates", 
            "6b. Evaluate efficacy and safety in prophylactic use",
            "6c. Promote adequate supply of therapeutics showing efficacy",
            "6d. Evaluate efficacy and safety of therapeutics through randomised clinical trials",
            "6e. Investigate combination therapies",
            "7a. Identification of candidates for clinical evaluation in addition to the ones already prioritized", 
            "7b. To develop and standardize animal models to evaluate the potential for vaccine effectiveness and to understand the potential for enhanced disease after vaccination. Results from animal models are expected to be important prior to large-scale efficacy studies and prior to studies in which enhanced disease is considered a significant possibility",
            "7c. To develop and standardize assays to support vaccine development, particularly to support the evaluation of immune responses and to support clinical case definition. Basic reagents should be shared to accelerate the development of international standards and reference panels that will help support the development of ELISAs, pseudovirion neutralization and PCR assays",
            "7d. To develop a multi-country Master Protocol for Phase 2b/Phase 3 vaccine evaluation to determine whether candidate vaccines are safe and effective before widespread distribution, using methodologically sound and ethically acceptable vaccine trial design. Vaccine efficacy trials should be done if such are feasible to implement",
            "7e. To develop potency assays and manufacturing processes to rapidly enable the production of high quality large quantities of clinical grade and GMP materials",
            "8a. Articulate and translate existing ethical standards to salient issues in COVID-19", 
            "8b. Sustained education, access, and capacity building",
            "8c. The impact of restrictive public health measures (e.g., quarantine, isolation, cordon sanitaire)",
            "8d. Public health communications and the ‘infodemic’; ensuring accurate and responsible communications",
            "8e. Ethical governance of global epidemic research",
            "9a. Public Health - What are relevant, feasible, effective approaches to promote acceptance, uptake, and adherence to public health measures for COVID-19 prevention and control; and how can secondary impacts be rapidly identified and mitigated?", 
            "9b. (Clinical) care and health Systems - What are the relevant, acceptable and feasible approaches for supporting the physical health and psychosocial needs of those providing care for COVID-19 patients?",
            "9c. Media and communication - How are individuals and communities communicating and making sense of COVID-19? What are the most effective ways to address the underlying drivers of fear, anxieties, rumours, stigma regarding COVID-19, and improve public knowledge, awareness, and trust during the response?",
            "9d. Engagement - What are the relevant, acceptable and feasible approaches for rapid engagement and good participatory practice that includes communities in the public health response?",
            "9e. Sexual and reproductive health - What are the relevant, acceptable and feasible approaches to communicating uncertainty regarding mother to child transmission of COVID-19, and possible sexual transmission?",
            "9f. International cooperation - What international coordination mechanisms can optimize the international response to COVID-19?"))

# Format data for Tableau ----

# 1) Combine main and sub priority areas, then separate across rows

covid_data_split_1 <- covid_data_clean %>%
  select(`Unique database reference number`, `Primary WHO area`, `Primary WHO sub-area`,
         `Secondary WHO area`, `Secondary WHO sub-area`) %>% 
  mutate(`Primary WHO sub-area` = str_to_lower(`Primary WHO sub-area`),
         `Secondary WHO sub-area` = str_to_lower(`Secondary WHO sub-area`)) %>% 
  mutate(Primary = if_else(is.na(`Primary WHO sub-area`), `Primary WHO area`,
                           paste0(`Primary WHO area`, ", ", `Primary WHO sub-area`)),
         Secondary = if_else(is.na(`Secondary WHO sub-area`), `Secondary WHO area`,
                 paste0(`Secondary WHO area`, ", ", `Secondary WHO sub-area`))) %>% 
  separate_rows(Primary, sep = ",", convert = FALSE) %>%
  separate_rows(Secondary, sep = ",", convert = FALSE) %>%
  mutate(Primary = str_trim(Primary),
         Secondary = str_trim(Secondary)) %>% 
  unique()



# 2) Filter out invalid priorities or sub-priority numbers

covid_data_split_2 <- covid_data_split_1 %>% 
  mutate(Primary = if_else(!(Primary %in% who_area_names$number |
                               Primary %in% who_area_sub_names$number), 
                                    "", Primary),
         Secondary = if_else(!(Secondary %in% who_area_names$number |
                                 Secondary %in% who_area_sub_names$number), 
                           "", Secondary)) %>% 
  filter(!(Primary == "" & Secondary != "") | `Primary WHO area` == "N, A")



# 3) Create new variables for main and sub priority part

covid_data_split_3 <- covid_data_split_2 %>% 
                        mutate(primary_who_area = substr(Primary, 1, 1),
                               primary_who_sub_area = substr(Primary, 2, 2),
                               secondary_who_area = substr(Secondary, 1, 1),
                               secondary_who_sub_area = substr(Secondary, 2, 2))


# 4) Filter out orphaned WHO priorities (check if sub-priorities are present)

covid_data_split_4 <- covid_data_split_3 %>% 
                        filter(!(coalesce(str_detect(`Primary WHO sub-area`, primary_who_area), FALSE) &
                                 primary_who_sub_area %in% c("N/A", ""))) %>% 
                        filter(
                          #is.na(`Secondary WHO sub-area`) |
                               !(coalesce(str_detect(`Secondary WHO sub-area`, secondary_who_area), FALSE) &
                                             secondary_who_sub_area %in% c("N/A", ""))) 


# 5) Keep only WHO area columns

covid_data_split_5 <- covid_data_split_4 %>%
  select(`Unique database reference number`, Primary, Secondary) %>% 
  gather(focus, code, Primary:Secondary) %>% 
  rename(who_sub_area = code) %>% 
  unique() %>% 
  filter(who_sub_area != "")


# 6) Join on WHO area labels

covid_data_split_6 <- covid_data_split_5 %>%
  mutate(who_area = substr(who_sub_area, 1, 1),
         who_sub_area = if_else(!(who_sub_area %in% who_area_sub_names$number), "N/A",
                                who_sub_area)) %>% 
  left_join(who_area_sub_names, by = c("who_sub_area" = "number")) %>%
  rename(sub_area_name = label) 


# 7) Separate out area and sub area

covid_data_split_7 <- covid_data_split_6 %>% 
  mutate(who_sub_area = if_else(who_sub_area != "N/A",
                                substr(who_sub_area, 2, 2), who_sub_area)) %>% 
  left_join(who_area_names, by = c("who_area" = "number")) %>%
  rename(area_name = label)


# 8) Filter out blank records, join to original data
heatmap_projects_final <- covid_data_split_7 %>% 
  right_join(covid_data_clean, by = "Unique database reference number") %>% 
  mutate(focus = coalesce(focus, "Primary")) %>% 
  mutate(who_sub_area = coalesce(who_sub_area, "N/A"),
         who_area = coalesce(who_area, "N/A"))


# Write data to EC google drive -----

results_url <- "https://docs.google.com/spreadsheets/d/1LN9EeJOFQP9sQVCsgoXomhY-LW8Gs6tz7QPKwt5xnz8/edit#gid=1621339980"
results <- as_sheets_id(results_url)

results_sheet <- sheet_write(heatmap_projects_final,
                             ss = results,
                             sheet = "heatmap_data")
