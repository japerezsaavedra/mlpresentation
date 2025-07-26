# Script para generar cluster plot con PCA
library(ggplot2)
library(dplyr)
library(factoextra)

# Cargar datos
data(iris)

# Preparar datos
iris_scaled <- scale(iris[,1:4])
set.seed(123)
kmeans_result <- kmeans(iris_scaled, 3)

# Aplicar PCA
pca_result <- prcomp(iris_scaled, center = TRUE, scale. = TRUE)

# Crear dataframe con resultados
iris_pca <- data.frame(
  Dim1 = pca_result$x[,1],
  Dim2 = pca_result$x[,2],
  Cluster = as.factor(kmeans_result$cluster),
  Species = iris$Species
)

# Calcular porcentajes de varianza
var_explained <- (pca_result$sdev^2 / sum(pca_result$sdev^2)) * 100
dim1_var <- round(var_explained[1], 1)
dim2_var <- round(var_explained[2], 1)

# Crear el gráfico
cluster_plot_pca <- ggplot(iris_pca, aes(x = Dim1, y = Dim2, color = Cluster, shape = Cluster)) +
  geom_point(size = 3, alpha = 0.8) +
  stat_ellipse(aes(fill = Cluster), alpha = 0.1, geom = "polygon") +
  scale_color_manual(values = c("#e74c3c", "#2ecc71", "#3498db")) +
  scale_fill_manual(values = c("#e74c3c", "#2ecc71", "#3498db")) +
  scale_shape_manual(values = c(16, 17, 15)) + # círculo, triángulo, cuadrado
  labs(
    title = "Cluster Plot",
    x = paste0("Dim1 (", dim1_var, "%)"),
    y = paste0("Dim2 (", dim2_var, "%)"),
    color = "Cluster",
    shape = "Cluster"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.position = "right",
    panel.grid = element_line(color = "grey90"),
    panel.background = element_rect(fill = "white")
  ) +
  coord_fixed(ratio = 1)

# Guardar el gráfico
ggsave("cluster_plot_pca.png", cluster_plot_pca, 
       width = 10, height = 8, dpi = 300, bg = "white")

# También crear versión con factoextra para comparar
png("cluster_plot_factoextra.png", width = 1000, height = 800)
fviz_cluster(kmeans_result, data = iris_scaled,
             geom = "point",
             ellipse.type = "convex",
             palette = c("#e74c3c", "#2ecc71", "#3498db"),
             ggtheme = theme_minimal(),
             main = "Cluster Plot - K-means con PCA")
dev.off()

print(paste("Gráficos generados:"))
print(paste("- Dim1 explica", dim1_var, "% de la varianza"))
print(paste("- Dim2 explica", dim2_var, "% de la varianza"))
print("Archivos guardados: cluster_plot_pca.png y cluster_plot_factoextra.png") 