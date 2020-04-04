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

# Get eigenvalues of principal components (> 1)
eig <- (pca$sdev)^2

# Get variances of principal components
var <- eig*100/sum(eig)

# Get cumulative variances
cumvar <- cumsum(var)

### End ###

# Combine to dataframe
eig_var_cum <- data.frame(eig = eig, variance = var, cumvariance = cumvar)

### Principal Component Analysis with principal function ###
# Load psych package
library(psych)

# Run PCA
principal <- principal(data2[, 16:29], nfactors = 6, rotate = "varimax")

### End ###

### Heatmap of factor loadings ###
# Load relevant packages
library(gplots)
library(RColorBrewer)

# Sort loadings
principal$loadings <- fa.sort(principal$loadings)

# Create heatmap 
heatmap.2(principal$loadings, trace = "none", dend = "none", main = "DR/WBR playing style profiles", 
          density.info = "none", breaks = seq(-1, 1, by = .2), margins = c(8, 12), cexCol = 1.2, cexRow = 1, 
          Colv = FALSE, Rowv = FALSE,
          sepwidth = c(0.025, 0.025), sepcolor = "white", colsep = 1:ncol(principal$loadings), rowsep = 1:nrow(principal$loadings),
          srtCol = 45,
          col = RColorBrewer::brewer.pal(10, 'RdBu'))

### End ###

### Cluster Analysis ###
d_data <- dist(na.omit(data2[, 16:29]), method = "euclidean")
hc_data <- hclust(d_data, method = "ward.D2") 
plot(hc_data, hang = -1, cex = 1)

