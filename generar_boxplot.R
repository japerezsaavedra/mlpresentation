# Script para generar boxplot con etiquetas horizontales
library(ggplot2)

# Cargar datos
data(iris)

# Crear boxplot con ggplot2 para mejor control de las etiquetas
png("boxplot_variables.png", width=1000, height=600, res=120)

# Convertir datos a formato largo para ggplot2
iris_long <- reshape2::melt(iris[,1:4])

# Crear boxplot con etiquetas horizontales
ggplot(iris_long, aes(x = variable, y = value, fill = variable)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Distribución de Variables del Dataset IRIS",
       x = "Variables",
       y = "Valores (cm)",
       fill = "Variables") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 12),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.position = "none",
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray95")
  ) +
  scale_fill_brewer(palette = "Set2") +
  scale_x_discrete(labels = c("Sepal.Length" = "Largo del Sépalo",
                             "Sepal.Width" = "Ancho del Sépalo", 
                             "Petal.Length" = "Largo del Pétalo",
                             "Petal.Width" = "Ancho del Pétalo"))

dev.off()

print("¡Boxplot generado exitosamente con etiquetas horizontales!") 