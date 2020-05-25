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

# Change names of loadings
colnames(principal$loadings) <- c("Playing style 1", "Playing style 2", "Playing style 3", "Playing style 4", 
                            "Playing style 5", "Playing style 6")

### End ###

### Heatmap of factor loadings ### --> Deze verbeteren
# Load relevant packages
library(gplots)
library(RColorBrewer)

# Create heatmap 
heatmap.2(principal$loadings, trace = "none", dend = "none", main = "Globale speelstijl-profielen van rechtervleugel", 
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

# Create custom colors for clusters
cols_branches <- c("blue", "red", "yellow", "green", "pink", "purple",
                   "brown", "gray", "black", "firebrick", "aquamarine",
                   "deepskyblue", "darksalmon", "khaki", "lightgreen", "orange",
                   "peru", "seagreen", "tan", "yellowgreen", "violet")

# Highlight Eredivisie & KK-divisie players
highlight <- rownames(data3)[data3$Player %in% c("D. Dumfries", "J. Teze", "Y. Sugawara", "S. Dest", "N. Mazraoui",
                                                "L. Geertruida", "D. Dankerlui", "Julio Pleguezuelo", "G. Troupée",
                                                  "D. Abels", "N. Bakboord", "D. Zeefuik", "G. Hamer",
                                                    "J. Lelieveld", "L. Rota", "C. Essers")]

# Change colnames
data3 <- data3 %>% 
  rename(Speler = Player, Leeftijd = Age, Club = Team)

# Create alternative label for the plot
data3$Label <- paste0(data3$Speler, " (", data3$Club, ")")

# Save dendrogram as high-res png file
png("dend.png", units = "in", width = 12, height = 12, res = 600)

# Create dendrogram
par(cex = 0.45, mar = c(6, 0, 4, 0), oma = c(4, 0, 2, 0)) 
dend <- hc_data %>% 
  as.dendrogram %>%
  set("branches_k_color", value = cols_branches, k = 21) %>%
  color_labels(labels = highlight , col = "red") 
  
# Use player names as labels
labels(dend) <- data3$Speler[hc_data$order]

# Circlize dendrogram
dend_circlized <- circlize_dendrogram(dend, labels = TRUE) 

# Add title to dendrogram
title(main = "Spelers die op basis van data gelijkenissen vertonen in speelstijl", cex.main = 3,
      sub = "Positie: RB/RWB | Gespeelde minuten: > 650 in het huidige of vorige seizoen | Leeftijd: tot en met 23 jaar | Aantal spelers: 310 | Bron data: Wyscout.", cex.sub = 2)

# Close the device
dev.off()

# Find cluster of Dumfries
which(data3$Speler == "D. Dumfries")
clusters[60]
cluster_dumfries <- subset(data3, clusters == 7)

# Select columns for table
library(ggpubr)
table <- cluster_dumfries[, c("Speler", "Club", "Leeftijd")]

# Sort table by age
table <- table[order(-table$Leeftijd),]

# Save table as high-res png file
png("tabel.png", units = "in", width = 6, height = 6, res = 600)

# Create table with players in cluster
table <- ggtexttable(table, theme = ttheme("minimal"))

# Highlight Dumfries and scouted players
table <- table_cell_bg(table, row = 3, column = 2, linewidth = 5,
                     fill = "#e23d32", color = "#bf9946")

table <- table_cell_bg(table, row = 8, column = 2, linewidth = 5,
                     fill = "grey70", color = "grey30")
  
table <- table_cell_bg(table, row = 11, column = 2, linewidth = 5,
                       fill = "grey70", color = "grey30")

table <- table_cell_bg(table, row = 12, column = 2, linewidth = 5,
                       fill = "grey70", color = "grey30")

table

# Close the device
dev.off()

# Upload FoForTho logo
library(magick)
library(here)

logo_raw <- image_read("Logo FoForTho.png")
logo <- logo_raw %>%
  image_scale("50") %>%
  image_background("grey", flatten = TRUE) %>%
  image_border("grey", "600x10") %>%
  image_annotate("FoForTho | @fofortho", color = "white", size = 20, 
                 location = "+10+25", gravity = "northeast") 

# Call back the plot
table <- image_read(paste0(here("/"), "tabel.png"))

# Stack them on top of each other
final_plot_table <- image_append(image_scale(c(table, logo), "4000"), stack = TRUE)

## Save Plot
magick::image_write(
  image = final_plot_table, 
  path = here::here("tabel_met logo.png"))
