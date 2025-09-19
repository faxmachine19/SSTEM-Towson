library(tidyverse)    # Graphing, data manipulation
library(cluster)	    # Gower's distance

# Given data, list of ids of interest, and neighborhood size parameter epsilon,
# create a list where each id corresponds to a list of ids within epsilon distance
get_nbhd <- function(merged_df, ids_int, epsilon) {
  gower_dist <- daisy(merged_df[, -1], metric="gower")

  n <- attr(gower_dist, "Size") # Get number of entries; equivalent to nrow(merged_df)

  # Initialize lists for neighborhoods
  nbhs <- list()
  nbh_ids <- list()

  # For each id of interest, find its neighbors
  for(id in ids_int) {
    # daisy() returns a vectorized lower-triangular distance matrix without the diagonal;
    # gower_dist[n*(i-1) - i*(i-1)/2 + j-i] is dissimilarity between row i and column j

    # Set i to row number corresponding to id
    i <- which(merged_df$ANON_ID == id)

    # Need character instead of integer to use as list name
    idc <- as.character(id)

    # List with only zero to not fail initialization
    # Conveniently, when converting row number to id, zero is naturally dropped
    nbhs[[idc]] <- c(0)

    # For j < i, check row j compared with column i
    for(j in 1:(i-1)) {
      # If distance less than epsilon, add row number to list
      if( gower_dist[n*(j-1) - j*(j-1)/2 + i-j] < epsilon ) {
        nbhs[[idc]] <- c(nbhs[[idc]], j)
      }
    }

    # For i < j, check row i compared with column j
    for(j in (i+1):n) {
      # If distance less than epsilon, add row number to list
      if( gower_dist[n*(i-1) - i*(i-1)/2 + j-i] < epsilon ) {
        nbhs[[idc]] <- c(nbhs[[idc]], j)
      }
    }
  
    # Convert row numbers to ids
    ids <- merged_df[nbhs[[idc]], "ANON_ID"]

    # Collect all ids in list
    nbh_ids[[idc]] = ids
  }

  return(nbh_ids)
}


# Given list of (id : neighborhood of id, size epsilon), epsilon, and folder name,
# create a plot of x = end GPA for id, y = average end GPA of id's neighbors
# NOTE: Requires terms <- readRDS("TERMS.rds") to be run
plot_gpas <- function(all_nbhs, epsilon, dir_name) {

  # Get end GPA for all students
  end_gpa <- terms |> group_by(ANON_ID) |> 
	arrange(STRM) |> summarize(END_GPA = last(CUM_GPA))

  # Initialize lists of gpas
  gpas_int <- list()  # scholar gpas
  gpas_nbhd <- list() # neighborhood gpas

  for(idc in names(all_nbhs)) {
    # Get end GPA for student idc
    gpas_int[[idc]] <- end_gpa[end_gpa$ANON_ID == as.numeric(idc), "END_GPA"]
    # Get average end GPA for neighborhood of idc; no neighbors gives NaN
    gpas_nbhd[[idc]] <- mean( (end_gpa |> filter(ANON_ID %in% all_nbhs[[idc]]))$END_GPA )
  }

  # Get number of neighbors in each neighborhood
  lengths <- lapply(all_nbhs, length)

  # Dataframe that will be passed to ggplot
  gpas <- as.data.frame(cbind(unlist(gpas_int), unlist(gpas_nbhd), unlist(lengths))) |>
    rename(gpa = V1, gpa_nbhd = V2, n = V3)

  # Replace NaN (no neighbors) with zero
  gpas$gpa_nbhd[is.nan(gpas$gpa_nbhd)] <- 0

  # Create plot, fixed size of (0,4) on both axes, point size scaled by
  # number of neighbors, and add line y=x
  ggplot(gpas, aes(x=gpa, y=gpa_nbhd, size=n)) + 
    geom_point(alpha=.5) +
    labs(title = "GPA and Neighborhood GPA",
	   subtitle = paste0("Epsilon: ", epsilon)) + 
    xlim(2, 4) + 
    ylim(2, 4) + 
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color="red") + 
    xlab("Ending GPA of Scholar") + 
    ylab("Average Ending GPA of Neighborhood")
  ggsave(paste0(dir_name, "/GPA", epsilon, ".png"))
}



terms <- readRDS("TERMS.rds")
merged_df <- readRDS("MERGED_DF.rds")

# Get list of IDs for people who have ever been SSTEM scholars
s_stem <- (terms |> group_by(ANON_ID) |> summarize(S_STEM = any(S_STEM)) |> filter(S_STEM) )$ANON_ID



# Comparing to all students in dataset
for(epsilon in c(.2, .15, .1, .075, .05, .025, .01, .005)) {
  all_nbhs <- get_nbhd(merged_df, s_stem, epsilon)
  plot_gpas(all_nbhs, epsilon, "S_STEM_NBHD/ALL")
}



# Get only students eligible for, or in, S-STEM program
# This is defined as: one semester with all of: Pell eligible, current GPA >= 3.0, math major
eligible <- unique( (terms |> 
	group_by(ANON_ID) |> 
	arrange(STRM) |> 
	filter(CUR_GPA >= 3.0) |> 
	filter(grepl("MATH", MAJOR1) | grepl("MATH", MAJOR2) | grepl("MATH", MAJOR3)) |>
	filter(PELL_ELIG == "Yes") )$ANON_ID )
eligible_df <- merged_df |> filter(ANON_ID %in% eligible | ANON_ID %in% s_stem)

for(epsilon in c(.2, .15, .1, .075, .05, .025, .01, .005)) {
  all_nbhs <- get_nbhd(eligible_df, s_stem, epsilon)
  plot_gpas(all_nbhs, epsilon, "S_STEM_NBHD/ELIGIBLE")
}


# Get only math majors
math <- unique( (terms |> 
	group_by(ANON_ID) |> 
	filter(grepl("MATH", MAJOR1) | grepl("MATH", MAJOR2) | grepl("MATH", MAJOR3)) )$ANON_ID )
math_df <- merged_df |> filter(ANON_ID %in% math | ANON_ID %in% s_stem)

for(epsilon in c(.2, .15, .1, .075, .05, .025, .01, .005)) {
  all_nbhs <- get_nbhd(math_df, s_stem, epsilon)
  plot_gpas(all_nbhs, epsilon, "S_STEM_NBHD/MATH")
}


# Repeat the process, but first adjust S-STEM scholars to remove the award
# from their unmet need
adj_df <- readRDS("MERGED_DF_SSTEM_ADJ.rds")
elig_adj_df <- adj_df |> filter(ANON_ID %in% eligible | ANON_ID %in% s_stem)
math_adj_df <- adj_df |> filter(ANON_ID %in% math | ANON_ID %in% s_stem)

for(epsilon in c(.2, .15, .1, .075, .05, .025, .01, .005)) {
  all_nbhs <- get_nbhd(adj_df, s_stem, epsilon)
  plot_gpas(all_nbhs, epsilon, "S_STEM_NBHD/ADJ")
}

for(epsilon in c(.2, .15, .1, .075, .05, .025, .01, .005)) {
  all_nbhs <- get_nbhd(elig_adj_df, s_stem, epsilon)
  plot_gpas(all_nbhs, epsilon, "S_STEM_NBHD/ADJ_ELIG")
}

for(epsilon in c(.2, .15, .1, .075, .05, .025, .01, .005)) {
  all_nbhs <- get_nbhd(math_adj_df, s_stem, epsilon)
  plot_gpas(all_nbhs, epsilon, "S_STEM_NBHD/ADJ_MATH")
}



# --- t-tests

# Visually the best choice
epsilon = .15


# Given list of neighborhoods (output of get_nbhd), perform t-test
# between students of interest and their neighborhood averages
# NOTE: Requires terms <- readRDS("TERMS.rds") to be run
nbhd_t_test <- function(all_nbhs) {
  end_gpa <- terms |> group_by(ANON_ID) |> 
	arrange(STRM) |> summarize(END_GPA = last(CUM_GPA))

  # Initialize lists of gpas
  gpas_int <- list()  # scholar gpas
  gpas_nbhd <- list() # neighborhood gpas

  for(idc in names(all_nbhs)) {
    # Get end GPA for student idc
    gpas_int[[idc]] <- end_gpa[all_merged$ANON_ID == as.numeric(idc), "END_GPA"]
    # Get average end GPA for neighborhood of idc; no neighbors gives NaN
    gpas_nbhd[[idc]] <- mean( (end_gpa |> filter(ANON_ID %in% all_nbhs[[idc]]))$END_GPA )
  }

  t.test(unlist(gpas_int), unlist(gpas_nbhd))
}

# Calculate all t-tests and print to file

cat("----- t-tests for neighborhoods -----\n\n\n", file="S_STEM_NBHD/t_tests.txt")

all_nbhs_all      <- get_nbhd(merged_df, s_stem, epsilon)
cat("--- all students ---\n", file="S_STEM_NBHD/t_tests.txt", append=T)
capture.output(nbhd_t_test(all_nbhs_all), file="S_STEM_NBHD/t_tests.txt", append=T)

all_nbhs_elig     <- get_nbhd(eligible_df, s_stem, epsilon)
cat("\n\n--- eligible students ---\n", file="S_STEM_NBHD/t_tests.txt", append=T)
capture.output(nbhd_t_test(all_nbhs_elig), file="S_STEM_NBHD/t_tests.txt", append=T)

all_nbhs_math     <- get_nbhd(math_df, s_stem, epsilon)
cat("\n\n--- math students ---\n", file="S_STEM_NBHD/t_tests.txt", append=T)
capture.output(nbhd_t_test(all_nbhs_math), file="S_STEM_NBHD/t_tests.txt", append=T)

all_nbhs_adj      <- get_nbhd(adj_df, s_stem, epsilon)
cat("\n\n--- adjusted, all students ---\n", file="S_STEM_NBHD/t_tests.txt", append=T)
capture.output(nbhd_t_test(all_nbhs_adj), file="S_STEM_NBHD/t_tests.txt", append=T)

all_nbhs_adj_elig <- get_nbhd(elig_adj_df, s_stem, epsilon)
cat("\n\n--- adjusted, eligible ---\n", file="S_STEM_NBHD/t_tests.txt", append=T)
capture.output(nbhd_t_test(all_nbhs_adj_elig), file="S_STEM_NBHD/t_tests.txt", append=T)

all_nbhs_adj_math <- get_nbhd(math_adj_df, s_stem, epsilon)
cat("\n\n--- adjusted, math ---\n", file="S_STEM_NBHD/t_tests.txt", append=T)
capture.output(nbhd_t_test(all_nbhs_adj_math), file="S_STEM_NBHD/t_tests.txt", append=T)









