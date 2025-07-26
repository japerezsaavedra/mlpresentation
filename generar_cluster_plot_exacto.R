# Script para generar el cluster plot exactamente como la imagen
library(plyr)
library(dplyr)
library(factoextra)
library(ggplot2)
library(caret)

# Cargar datos
data(iris)

# Escalamiento BBDD para trabajar con kmeans (exactamente como en el código base)
irisScale = scale(iris[,-5])
set.seed(123)
fitK <- kmeans(irisScale, 3)

# Agregar cluster a la base de datos
iris$cluster <- fitK$cluster

# Crear cluster plot con factoextra - EXACTAMENTE como la imagen
png("cluster_plot_exacto.png", width=1000, height=800, res=150)
fviz_cluster(fitK, data = iris[,-c(5,6)], geom = c("point"), 
             main = "Cluster plot",
             palette = c("#e74c3c", "#2ecc71", "#3498db"),
             ggtheme = theme_minimal(),
             ellipse.type = "convex",
             ellipse.alpha = 0.3,
             ellipse.border.remove = FALSE,
             pointsize = 2,
             labelsize = 12,
             main.title = "Cluster plot",
             xlab = "Dim1 (73%)",
             ylab = "Dim2 (22.9%)")
dev.off()

# Verificar el accuracy correcto (44.00%)
iris$cluster <- factor(iris$cluster)
iris$clus <- mapvalues(iris$cluster, c("1","2","3"), c("setosa","versicolor" ,"virginica"))
Indicadores_KMEANS <- confusionMatrix(iris$Species, iris$clus)

print("=== CLUSTER PLOT EXACTO GENERADO ===")
print(paste("Accuracy del K-means (código base):", round(Indicadores_KMEANS$overall["Accuracy"] * 100, 2), "%"))
print("Matriz de confusión:")
print(Indicadores_KMEANS$table)
print("Archivo guardado: cluster_plot_exacto.png") 