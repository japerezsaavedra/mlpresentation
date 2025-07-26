# Script para generar el cluster plot correcto con accuracy 44.00%
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

# Crear cluster plot con factoextra (como en el código base)
png("cluster_plot_correcto.png", width=1000, height=800)
fviz_cluster(fitK, data = iris[,-c(5,6)], geom = c("point"), 
             main = "Cluster Plot - K-means (K=3)",
             palette = c("#e74c3c", "#2ecc71", "#3498db"),
             ggtheme = theme_minimal())
dev.off()

# Verificar el accuracy correcto (44.00%)
iris$cluster <- factor(iris$cluster)
iris$clus <- mapvalues(iris$cluster, c("1","2","3"), c("setosa","versicolor" ,"virginica"))
Indicadores_KMEANS <- confusionMatrix(iris$Species, iris$clus)

print("=== CLUSTER PLOT CORRECTO GENERADO ===")
print(paste("Accuracy del K-means (código base):", round(Indicadores_KMEANS$overall["Accuracy"] * 100, 2), "%"))
print("Matriz de confusión:")
print(Indicadores_KMEANS$table)
print("Archivo guardado: cluster_plot_correcto.png") 