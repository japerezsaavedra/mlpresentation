# Script que replica exactamente el código original del usuario para K-means
library(plyr)
library(dplyr)
library(factoextra)
library(caret)

# Cargar datos
data(iris)

# Escalamiento BBDD para trabajar con kmeans (exactamente como en el código original)
irisScale = scale(iris[,-5])
summary(irisScale)

# Creación del Modelo KMEANS
set.seed(123)
fitK <- kmeans(irisScale, 3) ## Genera Modelo ML no supervisado Kmeans/Identificar clases de las especies con valores numericos para un K=3

# Agregar cluster a la base de datos
iris$cluster <- fitK$cluster

# Crear gráfico de clusters
png("clusters_kmeans_original.png", width=800, height=600)
fviz_cluster(fitK, data = iris[,-c(5,6)], geom = c("point"))
dev.off()

# PREDICCIONES / ACCURACY (exactamente como en el código original)
iris$cluster <- factor(iris$cluster) ## tabla visual de las predicciones
iris$clus <- mapvalues(iris$cluster, c("1","2","3"), c("setosa","versicolor" ,"virginica")) ##Categoriza variables de decision str 
Indicadores_KMEANS <- confusionMatrix(iris$Species, iris$clus) ## entrega resumen del modelado, matriz de confusion y accuracy

# Mostrar resultados
print("=== RESULTADOS K-MEANS (Código Original) ===")
print(Indicadores_KMEANS$overall)
print("Accuracy del K-means:")
print(Indicadores_KMEANS$overall["Accuracy"] * 100)

# Mostrar matriz de confusión
print("Matriz de confusión:")
print(Indicadores_KMEANS$table)

# Crear cluster plot con PCA usando los datos escalados originales
pca_result <- prcomp(irisScale, center = TRUE, scale. = TRUE)

# Crear dataframe con resultados
iris_pca <- data.frame(
  Dim1 = pca_result$x[,1],
  Dim2 = pca_result$x[,2],
  Cluster = as.factor(fitK$cluster),
  Species = iris$Species
)

# Calcular porcentajes de varianza
var_explained <- (pca_result$sdev^2 / sum(pca_result$sdev^2)) * 100
dim1_var <- round(var_explained[1], 1)
dim2_var <- round(var_explained[2], 1)

# Crear el gráfico cluster plot
library(ggplot2)
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
ggsave("cluster_plot_pca_original.png", cluster_plot_pca, 
       width = 10, height = 8, dpi = 300, bg = "white")

print(paste("Gráficos generados con código original:"))
print(paste("- Dim1 explica", dim1_var, "% de la varianza"))
print(paste("- Dim2 explica", dim2_var, "% de la varianza"))
print("Archivos guardados: clusters_kmeans_original.png y cluster_plot_pca_original.png") 