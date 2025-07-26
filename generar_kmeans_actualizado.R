# Script para generar K-means con cluster plot por dimensión y accuracy correcto
library(ggplot2)
library(dplyr)
library(gridExtra)

# Cargar datos
data(iris)

# Preparar datos
iris_scaled <- scale(iris[,1:4])
set.seed(123)
kmeans_result <- kmeans(iris_scaled, 3)

# Calcular accuracy del K-means
# Mapear clusters a especies (esto es una aproximación)
# Asumimos que el cluster más frecuente para cada especie es la predicción correcta
cluster_species_mapping <- table(kmeans_result$cluster, iris$Species)
predicted_species <- apply(cluster_species_mapping, 1, function(x) names(which.max(x)))
names(predicted_species) <- 1:3

# Crear predicciones
kmeans_predictions <- predicted_species[kmeans_result$cluster]
kmeans_accuracy <- mean(kmeans_predictions == iris$Species) * 100

print(paste("Accuracy del K-means:", round(kmeans_accuracy, 2), "%"))

# Crear cluster plot por dimensión
iris_with_clusters <- data.frame(
  iris,
  Cluster = as.factor(kmeans_result$cluster)
)

# Gráfico 1: Sepal Length vs Sepal Width
p1 <- ggplot(iris_with_clusters, aes(x = Sepal.Length, y = Sepal.Width, color = Cluster)) +
  geom_point(size = 3, alpha = 0.7) +
  scale_color_manual(values = c("#3498db", "#e74c3c", "#2ecc71")) +
  labs(title = "Clusters K-means: Largo vs Ancho del Sépalo",
       x = "Largo del Sépalo", y = "Ancho del Sépalo") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

# Gráfico 2: Petal Length vs Petal Width
p2 <- ggplot(iris_with_clusters, aes(x = Petal.Length, y = Petal.Width, color = Cluster)) +
  geom_point(size = 3, alpha = 0.7) +
  scale_color_manual(values = c("#3498db", "#e74c3c", "#2ecc71")) +
  labs(title = "Clusters K-means: Largo vs Ancho del Pétalo",
       x = "Largo del Pétalo", y = "Ancho del Pétalo") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

# Gráfico 3: Sepal Length vs Petal Length
p3 <- ggplot(iris_with_clusters, aes(x = Sepal.Length, y = Petal.Length, color = Cluster)) +
  geom_point(size = 3, alpha = 0.7) +
  scale_color_manual(values = c("#3498db", "#e74c3c", "#2ecc71")) +
  labs(title = "Clusters K-means: Sépalo vs Pétalo (Largo)",
       x = "Largo del Sépalo", y = "Largo del Pétalo") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

# Gráfico 4: Sepal Width vs Petal Width
p4 <- ggplot(iris_with_clusters, aes(x = Sepal.Width, y = Petal.Width, color = Cluster)) +
  geom_point(size = 3, alpha = 0.7) +
  scale_color_manual(values = c("#3498db", "#e74c3c", "#2ecc71")) +
  labs(title = "Clusters K-means: Sépalo vs Pétalo (Ancho)",
       x = "Ancho del Sépalo", y = "Ancho del Pétalo") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

# Combinar gráficos en una cuadrícula
cluster_plot_completo <- grid.arrange(p1, p2, p3, p4, ncol = 2)

# Guardar el gráfico
ggsave("clusters_kmeans_completo.png", cluster_plot_completo, 
       width = 12, height = 10, dpi = 300)

# También guardar el gráfico original para mantener compatibilidad
png("clusters_kmeans.png", width=800, height=600)
plot(iris$Petal.Length, iris$Petal.Width, 
     col = c("#3498db", "#e74c3c", "#2ecc71")[kmeans_result$cluster],
     pch = 19, cex = 1.5,
     main = "Clusters K-means (K=3)",
     xlab = "Largo del Pétalo", ylab = "Ancho del Pétalo")
legend("topright", legend = paste("Cluster", 1:3), 
       col = c("#3498db", "#e74c3c", "#2ecc71"), pch = 19, cex = 1.2)
dev.off()

print(paste("Accuracy del K-means calculado:", round(kmeans_accuracy, 2), "%"))
print("Gráficos de clusters generados exitosamente") 