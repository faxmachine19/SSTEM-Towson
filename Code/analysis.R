library(tidyverse)    # Graphing, data manipulation
library(cluster)      # Clustering
library(networkD3)    # Sankey charts
library(webshot)	    # Save Sankey as .png
webshot::install_phantomjs() # May be needed every time, or just once?


source("clustering.R") # Functions for clustering, visualization, and decision trees


# ----- Analysis -----

merged_df <- readRDS("MERGED_DF.rds")

merged_df |>
  filter(MATR <= 2021) |> 
  cluster_data(c(), dir_name="GRAD_4YR")
make_trees("GRAD_4YR", dir_name="GRAD_4YR")


merged_df |>
  filter(MATR <= 2019) |>
  cluster_data(c(), dir_name="GRAD_6YR")
make_trees("GRAD_6YR", dir_name="GRAD_6YR")


merged_df |> 
  cluster_data(c(), dir_name="END_GPA")
make_trees("END_GPA", dir_name="END_GPA")




# Finding descriptive retention rates of S-STEM students versus eligible
# Retention: Was here Spring 2025 or graduated

s_stem <- (terms |> group_by(ANON_ID) |> summarize(S_STEM = any(S_STEM)) |> filter(S_STEM) )$ANON_ID

eligible <- unique( (terms |> 
	group_by(ANON_ID) |> 
	arrange(STRM) |> 
	filter(CUR_GPA >= 3.0) |> 
	filter(grepl("MATH", MAJOR1) | grepl("MATH", MAJOR2) | grepl("MATH", MAJOR3)) |>
	filter(PELL_ELIG == "Yes") )$ANON_ID )

terms_elig <- terms |> filter(ANON_ID %in% eligible)

terms_ret <- terms_elig |> 
  filter(!(ANON_ID %in% s_stem)) |> 
  group_by(ANON_ID) |> 
  summarize(IS_HERE = (max(STRM) == 1252),
		DID_GRAD = any(!is.na(COMPLETION_TERM)))

terms_ret |> 
  mutate(RET = IS_HERE | DID_GRAD) |> 
  group_by(RET) |> 
  count()



elig_nbhs <- unique(unlist(all_nbhs))

terms_elig <- terms |> filter(ANON_ID %in% elig_nbhs)

terms_ret <- terms_elig |> 
  filter(!(ANON_ID %in% s_stem)) |> 
  group_by(ANON_ID) |> 
  summarize(IS_HERE = (max(STRM) == 1252),
		DID_GRAD = any(!is.na(COMPLETION_TERM))) |> 
  mutate(RET = IS_HERE | DID_GRAD)

terms_ret |> 
  group_by(RET) |> 
  count()


# From above, we see 23 of 149 elig nbh drop,
# and 45 of 282 elig drop
# We know 1 of 24 S-STEM dropped

prop.test(x=c(1, 23), n=c(24, 149))
prop.test(x=c(1, 45), n=c(24, 282))


epsilon=.15
all_nbhs <- get_nbhd(eligible_df, s_stem, epsilon)


ret_nbhd <- list()
for(idc in names(all_nbhs)) {
  ret_nbhd[[idc]] <- mean( (terms_ret |> filter(ANON_ID %in% all_nbhs[[idc]]))$RET )
}


library(poibin)
# Set missing value to 0
ret_nbhd[["5220"]] <- 0
dist <- unlist(ret_nbhd)
ppoibin(1, 1-dist)

# Removing missing value
ret_nbhd_copy <- ret_nbhd
ret_nbhd_copy[["5220"]] <- NULL
dist <- unlist(ret_nbhd_copy)
ppoibin(1, 1-dist)








# ----- Clustering adjusted SSTEM points into old clusters

# NOTE: This initial clustering includes the unadjusted SSTEM points
merged_df <- readRDS("MERGED_DF.rds")
gower_dist <- daisy(merged_df[, -1], metric="gower")

sils <- c(NA)
for(i in 2:5) {
  pam_fit <- pam(gower_dist, diss=T, k=i)
  sils[i] <- pam_fit$silinfo$avg.width
}
plot(1:5, sils, xlab="k", ylab="silhouette width", 
	main="Silhouette Width for Varying k", type="b")
kmax <- 3

pam_fit <- pam(gower_dist, diss=T, k=kmax)
clustered_df <- merged_df |> mutate(cluster = pam_fit$clustering)
medoids <- clustered_df[pam_fit$medoids, ]

s_stem <- (terms |> group_by(ANON_ID) |> 
		summarize(S_STEM = any(S_STEM)) |> filter(S_STEM) )$ANON_ID

sstem_adj_df <- readRDS("MERGED_DF_SSTEM_ADJ.rds") |> filter(ANON_ID %in% s_stem)

sstem_newclust <- sstem_adj_df |> mutate(cluster = 
	which.min(daisy(rbind(sstem_adj_df[1,], medoids[, -10])[, -1], metric="gower")[1:kmax]) )
	
sstem_oldclust <- clustered_df |> filter(ANON_ID %in% s_stem)



# --- Create Sankey diagrams to compare flow between different sets of clusters
# NOTE: .html file changes when re-running program; will need to manually update

clusts <- merge(sstem_oldclust, sstem_newclust, by="ANON_ID") |> 
	select(oldclust = cluster.x, newclust = cluster.y) |> 
	mutate(oldclust = paste0("old", oldclust), 
		 newclust = paste0("new", newclust)) |>
	group_by(oldclust, newclust) |> 
 	count()

links <- clusts |> rename(source = oldclust, target = newclust, value = n) |> as.data.frame()

nodes <- data.frame(name = 
	c(as.character(links$source), as.character(links$target)) |> unique() )

links$IDsource <- match(links$source, nodes$name) - 1
links$IDtarget <- match(links$target, nodes$name) - 1

my_color <- 'd3.scaleOrdinal() .domain(["old1", "old3", "new1"]) .range(["red", "orange", "blue"])'

sankeyNetwork(Links = links, Nodes = nodes, Source = "IDsource",
	Target = "IDtarget", Value = "value", NodeID = "name", 
	sinksRight = F, colourScale = my_color,
	fontSize=36, nodeWidth=15)

webshot("file:///C:/Users/eolear2/AppData/Local/Temp/RtmpyIkkGx/viewhtml3c8c4706ea9/index.html", 
	"SankeyOldNewClusters.png", delay=.2)




temp_4yr_df <- readRDS("GRAD_4YR/CLUSTERED_DF.rds")
temp_6yr_df <- readRDS("GRAD_6YR/CLUSTERED_DF.rds")
diff_clusts4yr6yr <- merge(temp_4yr_df, temp_6yr_df, by="ANON_ID") |> 
	select(oldclust = cluster.x, newclust = cluster.y) |> 
	mutate(oldclust = paste0("4yr", oldclust), 
		 newclust = paste0("6yr", newclust)) |>
	group_by(oldclust, newclust) |> 
 	count()

links4yr6yr <- diff_clusts4yr6yr |> rename(source = oldclust, target = newclust, value = n) |> as.data.frame()
nodes4yr6yr <- data.frame(name = c(as.character(links4yr6yr$source), as.character(links4yr6yr$target)) |> unique() )
links4yr6yr$IDsource <- match(links4yr6yr$source, nodes4yr6yr$name) - 1
links4yr6yr$IDtarget <- match(links4yr6yr$target, nodes4yr6yr$name) - 1
sankeyNetwork(Links = links4yr6yr, Nodes = nodes4yr6yr, Source = "IDsource",
	Target = "IDtarget", Value = "value", NodeID = "name", 
	sinksRight = F, fontSize=30, nodeWidth=15)

webshot("file:///C:/Users/eolear2/AppData/Local/Temp/RtmpyIkkGx/viewhtml3c8c766040fd/index.html", 
	"Sankey4yr6yrClusters.png", delay=.2)




temp_4yr_df <- readRDS("GRAD_4YR/CLUSTERED_DF.rds")
temp_full_df <- readRDS("END_GPA/CLUSTERED_DF.rds")
diff_clusts4yrfull <- merge(temp_4yr_df, temp_full_df, by="ANON_ID") |> 
	select(oldclust = cluster.x, newclust = cluster.y) |> 
	mutate(oldclust = paste0("4yr", oldclust), 
		 newclust = paste0("full", newclust)) |>
	group_by(oldclust, newclust) |> 
 	count()

links4yrfull <- diff_clusts4yrfull |> rename(source = oldclust, target = newclust, value = n) |> as.data.frame()
nodes4yrfull <- data.frame(name = c(as.character(links4yrfull$source), as.character(links4yrfull$target)) |> unique() )
links4yrfull$IDsource <- match(links4yrfull$source, nodes4yrfull$name) - 1
links4yrfull$IDtarget <- match(links4yrfull$target, nodes4yrfull$name) - 1
sankeyNetwork(Links = links4yrfull, Nodes = nodes4yrfull, Source = "IDsource",
	Target = "IDtarget", Value = "value", NodeID = "name", 
	sinksRight = F, fontSize=24, nodeWidth=15)

webshot("file:///C:/Users/eolear2/AppData/Local/Temp/RtmpyIkkGx/viewhtml3c8c7952316d/index.html",
	"Sankey4yrFullClusters.png", delay=.2)





