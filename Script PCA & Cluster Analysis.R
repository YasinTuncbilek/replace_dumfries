# Load relevant packages
library(readxl)

# Load data
data <- read_excel("RB_Combi.xlsx")

# Get column numbers
data.frame(colnames(data))

# Get relevant columns
data2 <- data[c(1:4, 6, 8, 15:17, 20:24, 26:28, 43, 47, 54, 56, 59, 60, 79, 80, 84)]

# 1. Player
# 2. Team
# 3. Position
# 4. Age
# 6. Contract expires
# 8. Minutes played
# 15. Foot
# 16. Height
# 17. Weight

# 20. Def duels per 90
# 21. Def duels won %
# 22. Aerial duels per 90
# 23. Aerial duels won %
# 24. Tackles per 90
# 26. Tackle succ. %
# 27. Shots blocked per 90
# 28. Interceptions per 90

# 43. Shots per 90
# 47. Crosses per 90
# 54. Dribbles per 90
# 56. Off duels per 90
# 59. Progressive runs per 90

# 60. Passes per 90
# 79. Key passes per 90
# 80. Final 3rd passes per 90
# 84. Thru passes per 90

# Calculate defensive stats
data2$'Def duels won per 90' <- (data2$`Def duels per 90` * data2$`Def duels won %`)/100
data2$'Aerial duels won per 90' <- (data2$`Aerial duels per 90` * data2$`Aerial duels won %`)/100
data2$'Tackles won per 90' <- (data2$`Tackles per 90` * data2$`Tackle succ. %`)/100

### Principal Component Analysis with prcomp function ###
# Run PCA 
pca <- prcomp(na.omit(data2[, 16:29]), scale = TRUE)

# Plot PCA
plot(pca, type = "l")

# Get eigenvalues of principal components (> 1)
eig <- (pca$sdev)^2

# Get variances of principal components
var <- eig*100/sum(eig)

# Get cumulative variances
cumvar <- cumsum(var)

# Combine to dataframe
eig_var_cum <- data.frame(eig = eig, variance = var, cumvariance = cumvar)

# There are 6 factors with an eigenvalue of > 1

### Another method: Principal Component Analysis with principal function of psych package ###
# Load psych package
library(psych)

# Run PCA
principal <- principal(na.omit(data2[, 16:29]), nfactors = 6, rotate = "varimax")

# Sort loadings
principal$loadings <- fa.sort(principal$loadings)

### End ###

### Heatmap of factor loadings ### --> Deze verbeteren
# Load relevant packages
library(gplots)
library(RColorBrewer)

# Create heatmap 
heatmap.2(principal$loadings, trace = "none", dend = "none", main = "DR/WBR playing style profiles", 
          density.info = "none", breaks = seq(-1, 1, by = .2), margins = c(8, 12), cexCol = 1.2, cexRow = 1, 
          Colv = FALSE, Rowv = FALSE,
          sepwidth = c(0.025, 0.025), sepcolor = "white", colsep = 1:ncol(principal$loadings), rowsep = 1:nrow(principal$loadings),
          srtCol = 45,
          col = RColorBrewer::brewer.pal(10, 'RdBu'))

### End ###

### Cluster Analysis ###
# Select players who played > 650 minutes
data3 <- data2 %>%
  filter(`Minutes played` > 650)

# Scale data
data_scaled <- scale(data3[, 16:29])

# Run Cluster Analysis
d_data <- dist(data_scaled, method = "euclidean")
hc_data <- hclust(d_data, method = "complete") 

# Use cutreeDynamic to determine numbers of clusters
library(dynamicTreeCut)
clusters <- cutreeDynamic(hc_data, cutHeight = NULL, minClusterSize = 3, method = "hybrid", distM = as.matrix(d_data))
clusters

### Dendrogram ###
# Load relevant packages
library(dplyr)
library(dendextend)
library(circlize)

# Save as dendrogram
dend <- hc_data %>% as.dendrogram 

# Use player names as labels
labels(dend) <- data3$Player[hc_data$order]

# Create custom colors for clusters
cols_branches <- c("blue", "red", "yellow", "green", "pink", "purple",
                   "brown", "gray", "black", "azure", "aquamarine",
                   "deepskyblue", "darksalmon", "khaki", "lightgreen", "orange",
                   "peru", "seagreen", "tan", "yellowgreen", "violet")

# Provide colors to the branches and labels
dend <- dend %>% 
  color_branches(k = 21, col = cols_branches) %>%
  color_labels(col = "black")

par(cex = 0.55, mar = c(5, 8, 4, 1)) 
circlize_dendrogram(dend, labels = TRUE) 
title(main = "Playing style partners of players on RB/RWB-position")

