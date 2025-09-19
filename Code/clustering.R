library(tidyverse)    # Graphing, data manipulation
library(cluster)      # Clustering
library(rpart)        # Decision trees
library(rpart.plot)   # Decision tree graphics
library(caret)        # Confusion matrices
library(gridExtra)    # Making big graphics


# Takes a dataframe of demographic information, 
# a list of courses to add in, and a folder name to save results to
cluster_data <- function(merged_df, courses, dir_name) {
  # Create directory, throws warning but does not overwrite if already exists
  dir.create(file.path(getwd(), dir_name))
  
  # If we selected any courses, merge them with merged_df
  if(length(courses) != 0) {
    all_courses <- readRDS("ALL_COURSES.rds")
    select_courses <- all_courses[, grepl(paste(c("ANON_ID", courses), collapse="|"), 
						    colnames(all_courses))]

    select_courses <- select_courses |> mutate(across(matches("ATMPTS"), ~ factor(gsub("3|4|5|6", "3+", .x), ordered=T)))

    merged_df <- merge(merged_df, select_courses, by="ANON_ID")
  }

  # This is purely for visuals; can be removed to save space
  write.csv(merged_df, paste0(dir_name, "/MERGED_DF.csv"), row.names=F)
  

  # Now that data is prepared, we perform clustering

  # Compute Gower's distance for dataframe
  gower_dist <- daisy(merged_df[, -1], metric="gower")

  # Calculate average silhouette width for k=2,3,...,10, plot them
  sils <- c(NA)
  for(i in 2:10) {
    pam_fit <- pam(gower_dist, diss=T, k=i)
    sils[i] <- pam_fit$silinfo$avg.width
  }
  png(paste0(dir_name, "/SilhouetteWidthAnalysis.png"))
  plot(1:10, sils, xlab="k", ylab="silhouette width", 
	 main="Silhouette Width for Varying k", type="b")
  dev.off()

  # Take k which maximizes cluster dissimilarity
  # We use k<=5 only because we want our clusters to be understandable and communicable
  k_max <- which(sils == max(sils[2:5]))

  # Using selected k, cluster dataframe and add cluster ID column
  pam_fit <- pam(gower_dist, diss=T, k=k_max)
  clustered_df <- merged_df |>
    mutate(cluster = pam_fit$clustering)

  saveRDS(clustered_df, paste0(dir_name, "/CLUSTERED_DF.rds"))


  # Textual descriptions of all clusters

  # Create five number summaries for each cluster
  clustered_df_summary <- clustered_df |>
    group_by(cluster) |> 
    do(summary = summary(.))
  
  # Write the medoids of each cluster to file, followed by summaries
  cat("Medoids\n", file = paste0(dir_name, "/ClusterNotes.txt"))
  capture.output(merged_df[pam_fit$medoids, ], 
		     file = paste0(dir_name, "/ClusterNotes.txt"), append=T)
  cat("\n\nSummaries\n", file = paste0(dir_name, "/ClusterNotes.txt"), append=T)
  capture.output(clustered_df_summary$summary,
		     file = paste0(dir_name, "/ClusterNotes.txt"), append=T)
  
  describe_clusters(dir_name)
}


# Takes in folder name
# Accesses clustered data in folder and creates histogram of every variable by cluster
# Separate from cluster_data() to allow for faster graphical adjustments (no re-clustering)
describe_clusters <- function(dir_name) {
  clustered_df <- readRDS(paste0(dir_name, "/CLUSTERED_DF.rds"))

  k_max = max(clustered_df$cluster)

  # Get list of all variables to graph
  cols <- colnames(clustered_df)
  cols <- cols[! cols %in% c("ANON_ID", "cluster")] 

  # Convert cluster and MATR to factors for graphing purposes only
  graphing_df <- clustered_df |> 
	mutate(cluster = as.factor(cluster),
		 MATR = as.factor(MATR))

  all_graphs <- list()

  # For each variable, create a specified graph
  for(col in cols) {
    graph <- ggplot(graphing_df, aes(get(col), fill=cluster)) + 
	facet_grid(~ cluster) + # Each cluster graphed separately
	scale_color_discrete() + 
	labs(title=paste0("Histogram of ", col, " per cluster")) + 
	xlab(col) + 
	# Add cluster size in proper location
	geom_text(data=graphing_df |> count(cluster), 
		    aes(label=paste("Cluster size:", n), 
			  x=-Inf, y=Inf, hjust=0, vjust=1))
    # Numeric variables as histograms, categorical as bar graphs
    if(is.numeric(graphing_df[, col])) {
	graph <- graph + geom_histogram() # Default bin widths are fine for our purposes
    } else {
	graph <- graph + geom_bar() + 
		theme(axis.text.x = element_text(angle=90)) # Vertical text to avoid clutter
    }
    # For grade variables, include average GPA of cluster and count of non-missing
    if(grepl("GRADE", col)) {
      graph <- graph + 
	  geom_text(data=graphing_df |> 
				group_by(cluster) |>
				select(col) |>  
				summarize(AVG_GPA = mean(get(col), na.rm=T)),
			aes(label=paste("Cluster GPA:", round(AVG_GPA, digits=2)),
			    x=-Inf, y=Inf, hjust=0, vjust=2.5)) + 
	  geom_text(data=graphing_df |> 
				group_by(cluster) |> 
				filter(!is.na(get(col))) |> 
				count(),
			aes(label=paste("Class Takers:", n),
			    x=-Inf, y=Inf, hjust=0, vjust=4))
    }
    all_graphs[[col]] <- ggplotGrob(graph)
    # Width of image depends on number of clusters
    ggsave(paste0(dir_name, "/", col, "ClusterHist.png"), width=k_max * 3, height=4, dpi=300)
  }

  # Arrange all graphs vertically and save as a single image
  biggraph <- do.call(grid.arrange, c(all_graphs, nrow=length(all_graphs), 
				ncol=1, top=dir_name ))
  ggsave(paste0(dir_name, "/BigGraph.png"), 
		width=k_max * 3, height = length(all_graphs) * 4, dpi=300, biggraph, limitsize=FALSE)
}


# Take variable of interest, folder name; 
# creates clustered/unclustered decision trees in the folder
# Note: Assumes folder exists
make_trees <- function(v_int, dir_name) {
  # If all_merged already exists, read it
  if(file.exists(paste0(dir_name, "/ALL_MERGED.rds"))) { 

    print("Loading existing data")
    all_merged <- readRDS(paste0(dir_name, "/ALL_MERGED.rds"))
    k = max(all_merged$cluster)

  } else { # Otherwise, create it and save it to the folder

    clustered_df <- readRDS(paste0(dir_name, "/CLUSTERED_DF.rds"))  
    k = max(clustered_df$cluster)

    # Add scholarship information as predictor variable
    clustered_df <- merge(clustered_df, 
			        readRDS("TERMS.rds") |>
				    group_by(ANON_ID) |> 
				    summarize(S_STEM = any(S_STEM), # Only new S-STEM
						  HILL_LOPES = any(HILL_LOPES),
						  SFS = any(SFS) ),
				  by="ANON_ID")

    all_interest <- readRDS("ALL_VAR_INTEREST.rds")

    var_interest <- all_interest |> select(all_of(c("ANON_ID", v_int)))

    all_merged <- merge(clustered_df, var_interest, by="ANON_ID")
    saveRDS(all_merged, paste0(dir_name, "/ALL_MERGED.rds"))

  }

  # Determine regression vs classification tree based on response variable
  if(v_int == "END_GPA") {
    method <- "anova"
  } else {
    method <- "class"
  }


  # --- Unclustered Decision Tree ---

  # Set seed for reproducible results
  set.seed(123)

  # Take 80-20 train-test split of data
  train_ind <- sample(seq_len(nrow(all_merged)), size=floor(.8*nrow(all_merged)))
  train_data <- all_merged[train_ind, -1] |> 
	select(-c(cluster)) |> 
	mutate(GRAD_4YR = factor(GRAD_4YR, labels = c("Drop", "Grad")))
  test_data <- all_merged[-train_ind, -1] |> 
	select(-c(cluster)) |>
	mutate(GRAD_4YR = factor(GRAD_4YR, labels = c("Drop", "Grad")))

  # Hyperparameters from past work
  ctrl <- rpart.control(minsplit=200, cp=.0015, xval=10)

  tree_unc <- rpart(eval(paste0(v_int, "~.")), data=train_data, control=ctrl, method=method)

  # Customized tree graphic, extra and split.fun change between classification and regression
  png(paste0(dir_name, "/UnclusteredDecisionTree", v_int, ".png"),
	width=1080, height=1080, units="px")
  if(method == "class") {
    prp(tree_unc, main=paste("Unclustered Decision Tree,", v_int),
	type=2, fallen=T, branch=.3, round=0, leaf.round=9, 
	under.cex=1, box.palette="RdYlGn",
	branch.col="gray", branch.lwd=2,
	extra=108, cex.main=1.5,
	under=T, lt=" < ", ge=" >= ",
	node.fun = function(x,labs,digits,varlen) paste0(labs, "\nn=", x$frame$n),
	split.fun = function(x,labs,digits,varlen,faclen) gsub(" = ", ":\n", labs))
  }
  if(method == "anova") {
    prp(tree_unc, main=paste("Unclustered Decision Tree,", v_int),
	type=2, fallen=T, branch=.3, round=0, leaf.round=9, 
	under.cex=1, box.palette="RdYlGn",
	branch.col="gray", branch.lwd=2,
	extra=101, cex.main=1.5,
	under=T, lt=" < ", ge=" >= ",
	split.fun = function(x,labs,digits,varlen,faclen) gsub(" = ", ":\n", labs))
  }
  dev.off()

  # Formatted printing to file of tree summary
  cat("----- UNCLUSTERED -----\n", file=paste0(dir_name, "/TreeSummaries.txt"))
  capture.output(summary(tree_unc), file=paste0(dir_name, "/TreeSummaries.txt"), append=T)
  cat("\n\n\n\n\n", file=paste0(dir_name, "/TreeSummaries.txt"), append=T)


  # To evaluate the performance of the unclustered classification tree:
  if(method == "class") {
    cat("----- UNCLUSTERED TRAINING SET -----\n", 
	  file=paste0(dir_name, "/TreeMetrics.txt"))
    train_result <- as.factor(train_data[, v_int])
    fits <- predict(tree_unc, train_data, type="class")
    capture.output(confusionMatrix(train_result, fits),
			 file=paste0(dir_name, "/TreeMetrics.txt"), append=T)
    cat("\n", file=paste0(dir_name, "/TreeMetrics.txt"), append=T)

    cat("----- UNCLUSTERED DECISION TREE -----\n", 
	  file=paste0(dir_name, "/TreeMetrics.txt"), append=T)
    test_result <- as.factor(test_data[, v_int])
    preds <- predict(tree_unc, test_data, type="class")
    capture.output(confusionMatrix(test_result, preds),
			 file=paste0(dir_name, "/TreeMetrics.txt"), append=T)
    cat("\n\n\n", file=paste0(dir_name, "/TreeMetrics.txt"), append=T)
    
  }


  
  # --- Clustered Decision Trees --- 

  # Again, set seed and get 80-20 train-test split
  set.seed(123)
  train_data_cluster <- all_merged |>
    group_by(cluster) |>
    slice_sample(prop=.8) |>
    ungroup()
  test_data_cluster <- all_merged |> 
    filter(!(ANON_ID %in% train_data_cluster$ANON_ID)) |>
    select(-ANON_ID)
  train_data_cluster <- train_data_cluster[, -1]

  # Create list of decision trees, one per cluster
  trees <- list()
  for(i in 1:k) {
    trees[[i]] = rpart(eval(paste0(v_int, "~.")), 
			   data=train_data_cluster |> filter(cluster == i),
			   control=ctrl, method=method)
  }
  
  # For each decision tree, create graphic (same as unclustered)
  for(i in 1:k) {
    png(paste0(dir_name, "/Cluster", i, "DecisionTree", v_int, ".png"))
    if(method == "class") {
      prp(trees[[i]], main=paste0("Cluster", i, "Decision Tree,", v_int),
	  type=2, fallen=T, branch=.3, round=0, leaf.round=9, 
	  under.cex=1, box.palette="RdYlGn",
	  branch.col="gray", branch.lwd=2,
	  extra=105, cex.main=1.5,
	  under=T, lt=" < ", ge=" >= ",
	  node.fun = function(x,labs,digits,varlen) paste0(labs, "\nn=", x$frame$n),
	  split.fun = function(x,labs,digits,varlen,faclen) gsub(" = ", ":\n", labs))
    }
    if(method == "anova") {
      prp(trees[[i]], main=paste0("Cluster", i, "Decision Tree,", v_int),
	  type=2, fallen=T, branch=.3, round=0, leaf.round=9, 
	  under.cex=1, box.palette="RdYlGn",
	  branch.col="gray", branch.lwd=2,
	  extra=101, cex.main=1.5,
	  under=T, lt=" < ", ge=" >= ",
	  split.fun = function(x,labs,digits,varlen,faclen) gsub(" = ", ":\n", labs))
    }
    dev.off()
  }

  # Measuring performance of clustered classification trees
  if(method == "class") {
    for(i in 1:k) {
      cat(paste("----- CLUSTER", i, "-----\n"), file=paste0(dir_name, "/TreeSummaries.txt"), append=T)
      capture.output(summary(tree_unc), file=paste0(dir_name, "/TreeSummaries.txt"), append=T)
      cat("\n\n\n", file=paste0(dir_name, "/TreeSummaries.txt"), append=T)
 
      cat(paste("--- CLUSTER", i, "TRAINING SET ---\n"), 
  		file=paste0(dir_name, "/TreeMetrics.txt"), append=T)
      curr_train <- train_data_cluster
      train_result <- as.factor(pull(curr_train, v_int)) # Use pull() since train is tibble
      fits <- predict(trees[[i]], curr_train, type="class")
      capture.output(confusionMatrix(train_result, fits),
			   file=paste0(dir_name, "/TreeMetrics.txt"), append=T)
      cat("\n", file=paste0(dir_name, "/TreeMetrics.txt"), append=T)

      cat(paste("--- CLUSTER", i, "DECISION TREE ---\n"), 
		file=paste0(dir_name, "/TreeMetrics.txt"), append=T)
      curr_test <- test_data_cluster |> filter(cluster == i)
      test_result <- as.factor(curr_test[, v_int])
      preds <- predict(trees[[i]], curr_test, type="class")
      capture.output(confusionMatrix(test_result, preds),
			   file=paste0(dir_name, "/TreeMetrics.txt"), append=T)
    
    }

  }

}
