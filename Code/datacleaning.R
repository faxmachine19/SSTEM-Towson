# File designed to take raw data and apply basic transformations as desired,
# being run only once to save cleaned dataframes to .rds objects
# Includes adjusting for inflation, replacing missing data,
# ensuring data types are correct, and removing incorrect data

# NOTE: Currently, replaces missing Pell eligibility status with "Unknown"
# rather than keeping it missing
# This assumption may impact analysis - if using Gower's distance later,
# "Unknown" is considered different from "No" and "Yes" and similar to "Unknown",
# whereas NA is not considered in the distance calculation
# This changes the similarity of certain rows

library(tidyverse) # Data handling
library(quantmod)  # Inflation adjustment

demoraw <- read.csv("../MATH_DEMO_UPD_RES.csv")
termsraw <- read.csv("../MATH_TERMS_UPD.csv")
inccredsraw <- read.csv("../MATH_INC_CREDS_UPD.csv")

# Get CPI for All Urban Consumers from FRED, convert to yearly inflation factor
getSymbols("CPIAUCSL", src="FRED")
avg_cpi <- as.data.frame(apply.yearly(CPIAUCSL, colMeans))
yrly_cpi <- data.frame(year = seq(1947,2025),
			     cpi = avg_cpi$CPIAUCSL) |>
		  filter(year >= 2011) |>
		  # Set up conversion to 2024 USD
		  mutate(conversion = avg_cpi['2024',] / cpi) |>
		  rename(AID_YR = year)

# Takes no arguments, returns a cleaned version of demographic data
clean_demo <- function() {
  demo <- demoraw |> 
    # Convert empty string to missing value
    replace(demoraw == "", NA) |> 
    # Convert categorical variables to factor
    mutate(INC_STATUS = as.factor(INC_STATUS),
	     TRANSFER_STATUS = as.factor(TRANSFER_STATUS),
	     RACE_ETH = as.factor(RACE_ETH),
	     SEX = as.factor(SEX),
	     HS_CEEB_CODE = as.factor(HS_CEEB_CODE),
	     HS_NAME = as.factor(HS_NAME),
	     RESIDENCY = as.factor(RESIDENCY),
	     POSTAL = as.factor(POSTAL),
	     COUNTRY = as.factor(COUNTRY),
	     MD_COUNTY = as.factor(MD_COUNTY) ) |> 
    # Split ALEKS score columns into score and date via regex
    separate_wider_regex(MIN_ALEKS_DATE, patterns = c(
      MIN_ALEKS_SCORE = "\\d+", "-", MIN_ALEKS_DATE = ".*")) |> 
    separate_wider_regex(MAX_ALEKS_DATE, patterns = c(
      MAX_ALEKS_SCORE = "\\d+", "-", MAX_ALEKS_DATE = ".*")) |> 
    # Convert new columns to int/Date types
    mutate(MIN_ALEKS_SCORE = as.integer(MIN_ALEKS_SCORE),
           MAX_ALEKS_SCORE = as.integer(MAX_ALEKS_SCORE),
           MIN_ALEKS_DATE = as.Date(MIN_ALEKS_DATE, format="%m/%d/%Y"),
           MAX_ALEKS_DATE = as.Date(MAX_ALEKS_DATE, format="%m/%d/%Y")) |>
    rename(MATR = CY_MATR)

  # Manually fix known issues with STATE column for readability
  # Appears to be issue with reading characters; student from Vietnam
  demo[demo$STATE == "H? Ch\xed" & !is.na(demo$STATE), "STATE"] <- "Ho Chi Minh"
  # Non-updated dataset has STATE as CN-SH and country as USA;
  # Updated dataset has STATE as "54" and country as CHN
  demo[demo$STATE == "54" & !is.na(demo$STATE), "STATE"] <- "Shanghai"

  # Now that STATE is corrected, convert to a factor
  demo <- demo |> mutate(STATE = as.factor(STATE))

  demo
}

# Takes no arguments, returns a cleaned version of term data
clean_terms <- function() {
  terms <- termsraw |> 
    # Replace empty string with NA
    replace(termsraw == "", NA) |>
    # Convert categorical data to factors and binary data to Booleans 
    mutate(ON_CAMP_HOUSING = (ON_CAMP_HOUSING == "Yes"),
	     AID_YR = as.factor(AID_YR),
	     DID_COMP = (DID_COMP == "YES"),
	     S_STEM = (S_STEM == "Yes"),
	     HILL_LOPES = (HILL_LOPES == "Yes"),
	     SFS = (SFS == "Yes") ) |> 
    # Replace missing Pell eligibility values with "Unknown" factor level
    mutate(PELL_ELIG = as.factor(replace_na(PELL_ELIG, "Unknown"))) |>
    mutate_if(is.character, as.factor) |> 
    # Replace missing financial information with zero
	# NOTE: Replaces negative UNMET_NEED with zero but NOT negative FED_EFC
    mutate(FED_EFC = ifelse(is.na(FED_EFC), 0, FED_EFC),
	     UNMET_NEED = ifelse(is.na(UNMET_NEED) | UNMET_NEED < 0, 0, UNMET_NEED),
	     FED_YEAR_COA = replace_na(FED_YEAR_COA, 0),
	     OFFER = replace_na(OFFER, 0)) |> 
    # S_STEM column includes old and new program; convert to two columns based on date
    mutate(S_STEM_OLD = (S_STEM & STRM < 1200),
	     S_STEM = (S_STEM & STRM > 1200))

  # Adjust all financial factors for inflation
  terms2 <- merge(terms, yrly_cpi |> select(-cpi), by="AID_YR") |>
		  mutate(FED_EFC = FED_EFC * conversion,
			   UNMET_NEED = UNMET_NEED * conversion,
			   FED_YEAR_COA = FED_YEAR_COA * conversion,
			   OFFER = OFFER * conversion) |>
		  select(-conversion) |> 
		  arrange(ANON_ID)

  terms2
}


# Takes no arguments, returns a cleaned version of incoming credit data
clean_inccreds <- function() {
  # CLASS_EQ is NA unless test credit; SCORE is NA unless test score
  inccreds <- inccredsraw |>
    # Replace empty string with missing
    replace(inccredsraw == "", NA) |> 
    # Convert columns to factors or Booleans
    mutate(PRIOR_CREDIT = as.factor(PRIOR_CREDIT),
	     SUBJECT = as.factor(SUBJECT),
	     EARN_CREDIT = (EARN_CREDIT == "Y"),
	     REPEAT_CODE = as.factor(REPEAT_CODE),
	     TEST_COMPONENT = as.factor(TEST_COMPONENT) )
  inccreds
}


# Save all cleaned dataframes as .rds objects for later
saveRDS(clean_demo(), "DEMO.rds")
saveRDS(clean_terms(), "TERMS.rds")
saveRDS(clean_inccreds(), "INCCREDS.rds")



# We also want to have unmet need excluding the S-STEM award for those students
# Below, we approximate that information

# Define a dataframe of average amount awarded to all S-STEM students per semester,
# then adjust for inflation
sstem_award <- 
  data.frame(STRM = c(1214, 1222, 1224, 1232, 1234, 1242, 1244, 1252),
		 AID_YR = c(2022, 2022, 2023, 2023, 2024, 2024, 2025, 2025),
		 AWARD = c(4404.5, 4283.375, 4555.75, 4605.111, 4892, 5000, 3983.364, 4337.167)) |> 
  merge(yrly_cpi, by="AID_YR") |>
  mutate(AWARD = AWARD * conversion)

# Generate a version of terms including a column for pre-S-STEM unmet need
sstem_terms <- clean_terms() |> 
  mutate(MOD = ifelse(S_STEM, STRM, 0)) |>
  merge(sstem_award |> select(MOD = STRM, AWARD), by="MOD", all.x=T) |> 
  mutate(SSTEM_UNMET = ifelse(is.na(AWARD), UNMET_NEED, UNMET_NEED + AWARD))

saveRDS(sstem_terms, "SSTEM_TERMS.rds")

