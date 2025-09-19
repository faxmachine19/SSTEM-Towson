# This file takes basic cleaned data and transforms it to the desired end-state
# for analysis. Includes converting grades to numerics, summarizing term data
# to single values per student, and getting course information

# Assumptions: 
# - Since Pell eligibility and financial numbers can change
# term to term, we summarize these to one value per student by taking the 
# mode of PELL_ELIG and the NA-dropped mean of FED_EFC and UNMET_NEED
# - When collapsing grades, we take the highest grade point value, 
# which includes other codes by nature (Ex. 'W' = 2.0 > 'F' = 0.0)


library(tidyverse)        # Data handling

# ----- Load data

demo <- readRDS("DEMO.rds")
terms <- readRDS("TERMS.rds")
inc_creds <- readRDS("INC_CREDS.rds")


# ----- Define helper functions

# Function to convert a letter grade or non-grade code to 
# a numerical value
convert_grades <- function(x) {
   V <- rev(c(4.0,3.7,3.3,3.0,2.7,2.3,2.0,
 		  1.7,1.3,1.0,0.0,2.0,2.0,2.0,
 		  0.0,0.0,0.0,0.0,0.0,0.0,0.0,
		  2.0,0.0))

  L <- factor(x, levels = 
			 rev(c('A', 'A-','B+','B',  'B-','C+','C',
				 'C-','D+','D', 'F',  'PS','S', 'SX',
				 'W', 'FX','AU','AUX','U', 'I', 'UX',
				 'CRD', 'NCR',
				 NA)), ordered=T)
  V[L]
}

# Function to calculate mode of vector
# NOTE: In case of tie, this picks the smallest value among highest frequency
vec_mode <- function(x) {
  uniq_x <- unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}


# ----- Create all major response variables, store


# Function to compute the variables of interest and store for later
# NOTE: Uses COMPLETION_TERM because a degree conferred during non-enrolled term 
# does NOT show in DID_COMP column, but should in COMPLETION_TERM
var_interest <- terms |> 
  group_by(ANON_ID) |> 
  arrange(STRM, .by_group=T) |> 
  summarize(END_GPA = last(CUM_GPA),
		GRAD_4YR = (last(STRM) - first(STRM) <= 39) & (any(!is.na(COMPLETION_TERM))), 
		GRAD_6YR = (last(STRM) - first(STRM) <= 59) & (any(!is.na(COMPLETION_TERM))) )

saveRDS(var_interest, "ALL_VAR_INTEREST.rds")


# ----- Merge information of interest from terms and demo

# Group terms by ANON_ID, then take mean/mode of relevant columns
# Note: summarize selects only those columns and the grouping factor (ANON_ID)
summed_terms <- terms |> 
  filter(STRM < 1254) |> # Remove terms that have not happened yet
  group_by(ANON_ID) |> 
  summarize(PELL_ELIG = vec_mode(PELL_ELIG),
		FED_EFC = mean(FED_EFC),
		UNMET_NEED = mean(UNMET_NEED))

# Simplify enrollment type and race/ethnicity and select important factors
simple_demo <- demo |>
  mutate(ENROLLMENT_TYPE = if_else(INC_STATUS == "FIRST-TIME", INC_STATUS, TRANSFER_STATUS),
	   ENROLLMENT_TYPE = fct_collapse(ENROLLMENT_TYPE, OTHER = 
				c("TAF","TAP","TAS","TIN","TNT","TVT","USB")),
	   RACE_ETH = fct_collapse(RACE_ETH, OTHER=c("AM-IND/NAT-AK",
				"NAT HI/PAC IS", "TWO OR MORE", "UNKNOWN", "US NONRES"))) |>
  filter(INC_TERM < 1254) |> # Remove students who have not yet started
  select(c("ANON_ID", "MATR", "ENROLLMENT_TYPE", "RACE_ETH", "SEX", "AGE"))

merged_df <- merge(summed_terms, simple_demo, by="ANON_ID")

# Censor financial information of 95th or higher percentile to equal the 95th percentile
# Because distance uses the range of the variable, high outliers make the variable less impactful
ninetyfifthefc <- quantile(merged_df$FED_EFC, probs=c(.95))
merged_df$FED_EFC <- ifelse(merged_df$FED_EFC >= ninetyfifthefc, 
					ninetyfifthefc, merged_df$FED_EFC)
ninetyfifthunmet <- quantile(merged_df$UNMET_NEED, probs=c(.95))
merged_df$UNMET_NEED <- ifelse(merged_df$UNMET_NEED >= ninetyfifthunmet, 
					ninetyfifthunmet, merged_df$UNMET_NEED)

saveRDS(merged_df, "MERGED_DF.rds")


# Repeat the process from above, but using UNMET_NEED adjusted to exclude S_STEM awards

sstem_terms <- readRDS("SSTEM_TERMS.rds")

summed_terms <- sstem_terms |> 
  filter(STRM < 1254) |> # Remove terms that have not happened yet
  group_by(ANON_ID) |> 
  summarize(PELL_ELIG = vec_mode(PELL_ELIG),
		FED_EFC = mean(FED_EFC),
		UNMET_NEED = mean(SSTEM_UNMET))

merged_df <- merge(summed_terms, simple_demo, by="ANON_ID")

ninetyfifthefc <- quantile(merged_df$FED_EFC, probs=c(.95))
merged_df$FED_EFC <- ifelse(merged_df$FED_EFC >= ninetyfifthefc, 
					ninetyfifthefc, merged_df$FED_EFC)
ninetyfifthunmet <- quantile(merged_df$UNMET_NEED, probs=c(.95))
merged_df$UNMET_NEED <- ifelse(merged_df$UNMET_NEED >= ninetyfifthunmet, 
					ninetyfifthunmet, merged_df$UNMET_NEED)

saveRDS(merged_df, "MERGED_DF_SSTEM_ADJ.rds")








# ----- Get class information

# Select columns of interest and convert grade factors to numerics
terms_courses <- terms |>
  filter(STRM < 1254) |> # Drop terms that have not happened yet
  select(ANON_ID, CRSE1, GRADE1, CRSE2, GRADE2, CRSE3, GRADE3, 
	   CRSE4, GRADE4, CRSE5, GRADE5, CRSE6, GRADE6) |>
  mutate(GRADE1 = convert_grades(GRADE1),
	   GRADE2 = convert_grades(GRADE2),
	   GRADE3 = convert_grades(GRADE3),
	   GRADE4 = convert_grades(GRADE4),
	   GRADE5 = convert_grades(GRADE5),
	   GRADE6 = convert_grades(GRADE6))


# Get maximum grade and number of attempts per course in each slot,
# then convert those course names to two columns:
# one for best grade, one for number of attempts within that slot
terms_course1 <- terms_courses |> 
  group_by(ANON_ID, CRSE1) |>
  summarize(GRADE1 = max(GRADE1), 
		ATMPTS1 = n()) |>
  pivot_wider(
    names_from = CRSE1,
    values_from = c(GRADE1, ATMPTS1),
    names_glue = "{CRSE1}_{.value}"
  ) |> 
  select(-c(NA_GRADE1, NA_ATMPTS1))

terms_course2 <- terms_courses |> 
  group_by(ANON_ID, CRSE2) |>
  summarize(GRADE2 = max(GRADE2),
		ATMPTS2 = n()) |>
  pivot_wider(
    names_from = CRSE2,
    values_from = c(GRADE2, ATMPTS2),
    names_glue = "{CRSE2}_{.value}"
  ) |> 
  select(-c(NA_GRADE2, NA_ATMPTS2))

terms_course3 <- terms_courses |> 
  group_by(ANON_ID, CRSE3) |>
  summarize(GRADE3 = max(GRADE3), 
		ATMPTS3 = n()) |>
  pivot_wider(
    names_from = CRSE3,
    values_from = c(GRADE3, ATMPTS3),
    names_glue = "{CRSE3}_{.value}"
  ) |> 
  select(-c(NA_GRADE3, NA_ATMPTS3))

terms_course4 <- terms_courses |> 
  group_by(ANON_ID, CRSE4) |>
  summarize(GRADE4 = max(GRADE4), 
		ATMPTS4 = n()) |>
  pivot_wider(
    names_from = CRSE4,
    values_from = c(GRADE4, ATMPTS4),
    names_glue = "{CRSE4}_{.value}"
  ) |> 
  select(-c(NA_GRADE4, NA_ATMPTS4))

terms_course5 <- terms_courses |> 
  group_by(ANON_ID, CRSE5) |>
  summarize(GRADE5 = max(GRADE5), 
		ATMPTS5 = n()) |>
  pivot_wider(
    names_from = CRSE5,
    values_from = c(GRADE5, ATMPTS5),
    names_glue = "{CRSE5}_{.value}"
  ) |> 
  select(-c(NA_GRADE5, NA_ATMPTS5))

terms_course6 <- terms_courses |> 
  group_by(ANON_ID, CRSE6) |>
  summarize(GRADE6 = max(GRADE6), 
		ATMPTS6 = n()) |>
  pivot_wider(
    names_from = CRSE6,
    values_from = c(GRADE6, ATMPTS6),
    names_glue = "{CRSE6}_{.value}"
  ) |> 
  select(-c(NA_GRADE6, NA_ATMPTS6))


# Combine all dataframes for individual course slots
# This is now a dataframe with hundreds of columns; one best grade and one # attempts
# for each unique course in each slot
merged_courses_wide <- 
  purrr::reduce(list(terms_course1, terms_course2, terms_course3,
			   terms_course4, terms_course5, terms_course6),
		    merge, by="ANON_ID")


# ----- Create .csv with all courses in sorted order

# Takes dataframe and course name, creates column with maximum grade across all course slots,
# then deletes slotwise columns
add_course <- function(df, name) {
  df <- df |>
    mutate(!!paste(name, "GRADE", sep="_") := 
		do.call(pmax, c(across(matches(paste0(name, "S?_GRADE"))), na.rm=T))) |>
    select(-matches(paste0(name, "S?_GRADE[1-6]")))
  return(df)
}

# Takes dataframe and course name, creates column with sum of attempts across all course slots,
# then deletes slotwise columns
add_atmpts <- function(df, name) {
  df <- df |>
    mutate(!!paste(name, "ATMPTS", sep="_") :=
		rowSums(across(matches(paste0(name,"S?_ATMPTS"))), na.rm=T)) |>
    select(-matches(paste0(name, "S?_ATMPTS[1-6]")))
  return(df)
}

# Given dataframe and list of courses, return dataframe with ANON_ID and consolidated
# grade and attempt count for only the courses provided
get_courses <- function(df, courses) {
  for(name in courses) {
    df <- add_course(df, name)
    df <- add_atmpts(df, name)
  }
  # Select columns that DO NOT end in a digit; should be ANON_ID and created columns
  return(df |> select(-matches("[1-6]$")))
}


# Get list of all courses in dataset, sort by course number
course_list <- unique(c(levels(terms$CRSE1), levels(terms$CRSE2),
			      levels(terms$CRSE3), levels(terms$CRSE4),
			      levels(terms$CRSE5), levels(terms$CRSE6)))
course_list <- sort(course_list)
# Manually move MATH95 to the front, since this is a 0xx level course
course_list <- course_list[course_list != "MATH95"]
course_list <- c("MATH95", course_list)
# Removes courses with S suffix via regex
course_list <- course_list[!grepl("\\dS", course_list)]

merged_courses <- get_courses(merged_courses_wide, course_list)

saveRDS(merged_courses, file="ALL_COURSES.rds")


