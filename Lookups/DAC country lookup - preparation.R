# Script to create DAC Income Group country lookup 
# (based on list of Tableau accepted country spellings)

# Prepare Tableau and DAC Income Group lookups

tableau_list <- read_excel("Archive/Tableau countries.xlsx")
dac_lookup <- read_excel("Archive/DAC country lookup.xlsx")

# Join DAC lookup to Tableau list
tableau_dac_combined <- tableau_list %>%
                           left_join(dac_lookup, by = c("Country (Name)" = "Recipient name")) %>%
                           select(country_code = `Country 2 char (ISO 3166-1)`,
                                  `DAC Income Group`) %>%
                           filter(!is.na(`DAC Income Group`)) %>%
                           unique()

# Join back on to Tableau list of accepted country spellings
tableau_dac_final <- tableau_list %>%
                           left_join(tableau_dac_combined, by = c("Country 2 char (ISO 3166-1)" = "country_code")) %>%
                            select(country_name = `Country (Name)`,
                                   country_code = `Country 2 char (ISO 3166-1)`,
                                   `DAC Income Group`)


# Save lookup file
write_xlsx(tableau_dac_final, "Lookups/Country lookup - Tableau and DAC Income Group.xlsx")
