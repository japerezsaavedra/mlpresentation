# Script para explicar detalladamente el accuracy del K-means
library(dplyr)

# Cargar datos
data(iris)

# Preparar datos
iris_scaled <- scale(iris[,1:4])
set.seed(123)
kmeans_result <- kmeans(iris_scaled, 3)

# Mostrar los clusters encontrados
cat("=== CLUSTERS ENCONTRADOS POR K-MEANS ===\n")
print(table(kmeans_result$cluster, iris$Species))

# Calcular el mapeo de clusters a especies
cluster_species_mapping <- table(kmeans_result$cluster, iris$Species)
cat("\n=== MAPEO DE CLUSTERS A ESPECIES ===\n")
print(cluster_species_mapping)

# Encontrar la especie más frecuente en cada cluster
predicted_species <- apply(cluster_species_mapping, 1, function(x) names(which.max(x)))
names(predicted_species) <- 1:3
cat("\n=== ESPECIE PREDOMINANTE EN CADA CLUSTER ===\n")
print(predicted_species)

# Crear predicciones
kmeans_predictions <- predicted_species[kmeans_result$cluster]

# Calcular accuracy
correct_predictions <- sum(kmeans_predictions == iris$Species)
total_flowers <- nrow(iris)
accuracy <- (correct_predictions / total_flowers) * 100

cat("\n=== CÁLCULO DEL ACCURACY ===\n")
cat("Flores clasificadas correctamente:", correct_predictions, "\n")
cat("Total de flores:", total_flowers, "\n")
cat("Accuracy:", round(accuracy, 2), "%\n")

# Mostrar detalle por especie
cat("\n=== DETALLE POR ESPECIE ===\n")
for(species in unique(iris$Species)) {
  species_data <- iris$Species == species
  species_correct <- sum(kmeans_predictions[species_data] == species)
  species_total <- sum(species_data)
  species_accuracy <- (species_correct / species_total) * 100
  cat(species, ":", species_correct, "/", species_total, "=", round(species_accuracy, 2), "%\n")
}

# Crear tabla de confusión
confusion_table <- table(Predicted = kmeans_predictions, Actual = iris$Species)
cat("\n=== TABLA DE CONFUSIÓN ===\n")
print(confusion_table)

# Explicación final
cat("\n=== EXPLICACIÓN ===\n")
cat("El K-means encontró 3 grupos basándose solo en las medidas físicas.\n")
cat("Al mapear cada cluster a la especie más frecuente, 125 de 150 flores\n")
cat("fueron clasificadas correctamente, dando un accuracy de 83.33%.\n")
cat("Esto indica que las características físicas de las flores iris\n")
cat("están muy bien separadas por especie, permitiendo una buena\n")
cat("clasificación incluso sin conocer las etiquetas de especie.\n") 