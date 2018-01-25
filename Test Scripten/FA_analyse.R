#if(!require(devtools)) install.packages("devtools")
#devtools::install_github("kassambara/factoextra")
#Load factoextra as follow :
  library("factoextra")
data("decathlon2")
df <- decathlon2[1:23, 1:10]

library("FactoMineR")
res.pca <- PCA(df,  graph = FALSE)

get_eig(res.pca)

# Visualize eigenvalues/variances
fviz_screeplot(res.pca, addlabels = TRUE, ylim = c(0, 50))

#4.Extract and visualize results for variables:

  # Extract the results for variables
  var <- get_pca_var(res.pca)
var


# Coordinates of variables
head(var$coord)


# Contribution of variables
head(var$contrib)


# Graph of variables: default plot
fviz_pca_var(res.pca, col.var = "black")


# Control variable colors using their contributions
fviz_pca_var(res.pca, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
)


# install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")
?chart.Correlation#(decathlon2.active[, 1:6], histogram=TRUE, pch=19)

data(managers)
chart.Correlation(managers[,1:8], histogram=TRUE, pch="+")

