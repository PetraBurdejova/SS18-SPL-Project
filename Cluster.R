###############################################################################
############################ Clustering techniques ############################
###############################################################################

####################### Distance/Dissimilarity matrix #########################

gower_dist <- daisy(sample[, -1],
                    metric = "gower")

summary(gower_dist) # summary statistics

# Change type
gower_mat <- as.matrix(gower_dist) 

# Most similar pair of data 
sample[
  which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]),
        arr.ind = TRUE)[1, ], -1]

# Most dissimilar pair of data 
sample[
  which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]),
        arr.ind = TRUE)[1, ], -1]


###################### Determining number of clusters #########################

# Silhouette width 

sil_width <- c(NA)

for(i in 2:11){
  pam_fit <- pam(gower_dist,
                 diss = TRUE,
                 k = i)
  sil_width[i] <- pam_fit$silinfo$avg.width
}

# Plot silhuette width in number of clusters

plot(1:11, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width") # optimal number of clusters 8
lines(1:11, sil_width)


########################### Partitioning clustering ###########################

# PAM-algorithm 

pam_fit <- pam(gower_dist, diss = TRUE, k = 8)

pam_results <-  sample%>%
  dplyr::select(-ID) %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))

pam_results$the_summary

sample[pam_fit$medoids, ]

# Cluster visualization
tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)

tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering),
         name = sample$state)

ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))


# K-modes algorithm 
cl <- kmodes(sample[, -1], 8)
plot(sample[, -1],col= cl$cluster)

hclust(gower_dist)

# Cluster visualization

########################### Hierarchical clustering ###########################
# Divisive clustering 
divisive.clust <- diana(as.matrix(gower_dist), 
                        diss = TRUE, keep.diss = TRUE)
plot(divisive.clust, main = "Divisive")


# Agglomerative clustering 
aggl.clust.c <- hclust(gower_dist, method = "complete")
plot(aggl.clust.c,
     main = "Agglomerative, complete linkages")
rect.hclust(aggl.clust.c, k = 8, border = "red")
d2 <- color_branches(aggl.clust.c,k=8) # auto-coloring 5 clusters of branches.
plot(d2)


########################## Density based clustering ###########################

# DBSCAN

# Find eps
windows()
layout(matrix(1:2, nrow=1))
plot(density(na.omit(gower_dist[upper.tri(gower_dist)])), main="kernel density")
plot(ecdf(gower_dist[upper.tri(gower_dist)]), main="ECDF")

# DBSCAN algorithm
sb <- dbscan(gower_dist, eps = .2, MinPts = 5, method="dist")

# Cluster visualization


################################# Comparison ##################################



