# Script para verificar el mapeo de clusters del código base
library(plyr)
library(dplyr)
library(caret)

# Cargar datos
data(iris)

# Escalamiento BBDD para trabajar con kmeans (exactamente como en el código original)
irisScale = scale(iris[,-5])
set.seed(123)
fitK <- kmeans(irisScale, 3)

# Agregar cluster a la base de datos
iris$cluster <- fitK$cluster

# Ver la distribución original de clusters vs especies
cat("=== DISTRIBUCIÓN ORIGINAL DE CLUSTERS ===\n")
print(table(fitK$cluster, iris$Species))

# PREDICCIONES / ACCURACY (exactamente como en el código original)
iris$cluster <- factor(iris$cluster)
iris$clus <- mapvalues(iris$cluster, c("1","2","3"), c("setosa","versicolor" ,"virginica"))
Indicadores_KMEANS <- confusionMatrix(iris$Species, iris$clus)

cat("\n=== MAPEO APLICADO ===\n")
cat("Cluster 1 -> setosa\n")
cat("Cluster 2 -> versicolor\n")
cat("Cluster 3 -> virginica\n")

cat("\n=== MATRIZ DE CONFUSIÓN ===\n")
print(Indicadores_KMEANS$table)

cat("\n=== ACCURACY ===\n")
cat("Accuracy:", round(Indicadores_KMEANS$overall["Accuracy"] * 100, 2), "%\n")

# Verificar si el mapeo es correcto
cat("\n=== VERIFICACIÓN DEL MAPEO ===\n")
for(i in 1:3) {
  cluster_data <- iris[fitK$cluster == i, ]
  species_counts <- table(cluster_data$Species)
  dominant_species <- names(which.max(species_counts))
  cat("Cluster", i, "-> Especie dominante:", dominant_species, "(", max(species_counts), "de", sum(species_counts), ")\n")
} 