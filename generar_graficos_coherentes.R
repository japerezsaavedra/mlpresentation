# Script para generar todos los gráficos con paleta de colores coherente
library(caret)
library(randomForest)
library(rpart)
library(rpart.plot)
library(e1071)
library(kknn)
library(nnet)
library(dplyr)
library(ggplot2)
library(reshape2)

# Definir paleta de colores moderna y coherente
color_palette <- c("#3498db", "#e74c3c", "#2ecc71", "#f39c12", "#9b59b6", "#1abc9c")
color_palette_light <- c("#85c1e9", "#f1948a", "#82e0aa", "#f8c471", "#bb8fce", "#76d7c4")

# Cargar datos
data(iris)

# Normalizar datos
norm <- function(x){(x-min(x))/(max(x)-min(x))}
iris_norm <- data.frame(apply(iris[,-5], 2, norm))
iris_norm$Species <- iris$Species

# Dividir datos
set.seed(123)
training.samples <- iris_norm$Species %>% 
  createDataPartition(p = 0.8, list = FALSE)
train_data <- iris_norm[training.samples, ]
test_data <- iris_norm[-training.samples, ]

# Entrenar modelos
knn_model <- train.kknn(Species ~ ., data = train_data, kmax = 10)
svm_model <- svm(Species ~ ., data = train_data, kernel = "radial")
dt_model <- rpart(Species ~ ., data = train_data)
nb_model <- naiveBayes(Species ~ ., data = train_data)
rf_model <- randomForest(Species ~ ., data = train_data, ntree = 250)
nn_model <- nnet(Species ~ ., data = train_data, size = 5, decay = 5e-4, maxit = 200, trace = FALSE)

# Predicciones
pred_knn <- predict(knn_model, test_data)
pred_svm <- predict(svm_model, test_data)
pred_dt <- predict(dt_model, test_data, type = 'class')
pred_nb <- predict(nb_model, test_data)
pred_rf <- predict(rf_model, test_data)
pred_nn <- factor(predict(nn_model, test_data, type = 'class'))

# Matrices de confusión
cm_knn <- confusionMatrix(test_data$Species, pred_knn)
cm_svm <- confusionMatrix(test_data$Species, pred_svm)
cm_dt <- confusionMatrix(test_data$Species, pred_dt)
cm_nb <- confusionMatrix(test_data$Species, pred_nb)
cm_rf <- confusionMatrix(test_data$Species, pred_rf)
cm_nn <- confusionMatrix(test_data$Species, pred_nn)

# 1. Boxplot con ggplot2
png("boxplot_variables.png", width=1000, height=600, res=120)
iris_long <- melt(iris[,1:4])
ggplot(iris_long, aes(x = variable, y = value, fill = variable)) +
  geom_boxplot(alpha = 0.8) +
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
  scale_fill_manual(values = color_palette) +
  scale_x_discrete(labels = c("Sepal.Length" = "Largo del Sépalo",
                             "Sepal.Width" = "Ancho del Sépalo", 
                             "Petal.Length" = "Largo del Pétalo",
                             "Petal.Width" = "Ancho del Pétalo"))
dev.off()

# 2. Matriz de correlación
png("matriz_correlacion.png", width=800, height=600)
cor_matrix <- cor(iris[,1:4])
# Paleta personalizada con colores vibrantes
custom_palette <- c("#00876c", "#8dc281", "#fffaa6", "#f4a15d", "#d43d51")
corrplot::corrplot(cor_matrix, method = "color", type = "upper", 
                   col = colorRampPalette(custom_palette)(100),
                   tl.col = "black", tl.srt = 45, tl.cex = 1.2,
                   title = "Matriz de Correlación", mar = c(0,0,2,0),
                   addCoef.col = "black", number.cex = 1.2)
dev.off()

# 3. Screeplot PCA
png("screeplot_pca.png", width=800, height=600)
pca_result <- prcomp(iris[,1:4], scale. = TRUE)
plot(pca_result$sdev^2 / sum(pca_result$sdev^2) * 100, 
     type = "b", col = color_palette[1], lwd = 3,
     main = "Screeplot - Análisis de Componentes Principales",
     xlab = "Componente Principal", ylab = "Varianza Explicada (%)",
     pch = 19, cex = 1.5)
grid(col = "gray90")
dev.off()

# 4. Clusters K-means
png("clusters_kmeans.png", width=800, height=600)
set.seed(123)
kmeans_result <- kmeans(scale(iris[,1:4]), 3)
plot(iris$Petal.Length, iris$Petal.Width, 
     col = color_palette[kmeans_result$cluster],
     pch = 19, cex = 1.5,
     main = "Clusters K-means (K=3)",
     xlab = "Largo del Pétalo", ylab = "Ancho del Pétalo")
legend("topright", legend = paste("Cluster", 1:3), 
       col = color_palette[1:3], pch = 19, cex = 1.2)
dev.off()

# 5. Árbol de decisión
png("arbol_decision.png", width=1000, height=800)
rpart.plot(dt_model, main = "Árbol de Decisión - Dataset IRIS",
           box.palette = list(color_palette[1], color_palette[2], color_palette[3]), 
           shadow.col = "gray")
dev.off()

# 6. Importancia de variables RF
png("importancia_variables_rf.png", width=800, height=600)
varImpPlot(rf_model, main = "Importancia de Variables - Random Forest",
           col = color_palette[1], pch = 19, cex = 1.2)
dev.off()

# 7. Matrices de confusión
modelos_cm <- list(knn = cm_knn, svm = cm_svm, dt = cm_dt, 
                   nb = cm_nb, rf = cm_rf, nn = cm_nn)
nombres_cm <- c("KNN", "SVM", "Decision Tree", "Naive Bayes", "Random Forest", "Neural Network")

for(i in 1:length(modelos_cm)) {
  png(paste0("matriz_confusion_", names(modelos_cm)[i], ".png"), width=800, height=600)
  plot(modelos_cm[[i]]$table, main = paste("Matriz de Confusión -", nombres_cm[i]), 
       col = color_palette_light, text.col = "black", cex = 1.5)
  dev.off()
}

# 8. Comparación de accuracy
accuracies <- c(
  cm_knn$overall["Accuracy"],
  cm_svm$overall["Accuracy"],
  cm_dt$overall["Accuracy"],
  cm_nb$overall["Accuracy"],
  cm_rf$overall["Accuracy"],
  cm_nn$overall["Accuracy"]
) * 100

png("comparacion_accuracy.png", width=1200, height=700)
par(mar = c(8, 5, 4, 2) + 0.1)
barplot(accuracies, names.arg = nombres_cm, 
        col = color_palette, main = "Comparación de Accuracy por Modelo",
        ylab = "Accuracy (%)", ylim = c(0, 100), las = 2, cex.names = 1.2)
abline(h = 90, col = "#e74c3c", lty = 2, lwd = 3)
text(3.5, 92, "Línea de referencia (90%)", col = "#e74c3c", cex = 1.2)
dev.off()

# 9. Comparación de tiempos
st.time <- Sys.time()
knn_model <- train.kknn(Species ~ ., data = train_data, kmax = 10)
knn_time <- as.numeric(Sys.time() - st.time)

st.time <- Sys.time()
svm_model <- svm(Species ~ ., data = train_data, kernel = "radial")
svm_time <- as.numeric(Sys.time() - st.time)

st.time <- Sys.time()
dt_model <- rpart(Species ~ ., data = train_data)
dt_time <- as.numeric(Sys.time() - st.time)

st.time <- Sys.time()
nb_model <- naiveBayes(Species ~ ., data = train_data)
nb_time <- as.numeric(Sys.time() - st.time)

st.time <- Sys.time()
rf_model <- randomForest(Species ~ ., data = train_data, ntree = 250)
rf_time <- as.numeric(Sys.time() - st.time)

st.time <- Sys.time()
nn_model <- nnet(Species ~ ., data = train_data, size = 5, decay = 5e-4, maxit = 200, trace = FALSE)
nn_time <- as.numeric(Sys.time() - st.time)

tiempos <- c(knn_time, svm_time, dt_time, nb_time, rf_time, nn_time)

png("comparacion_tiempos.png", width=1200, height=700)
par(mar = c(8, 5, 4, 2) + 0.1)
barplot(tiempos, names.arg = nombres_cm, 
        col = color_palette, main = "Comparación de Tiempos de Entrenamiento",
        ylab = "Tiempo (segundos)", las = 2, cex.names = 1.2)
dev.off()

print("¡Todos los gráficos han sido generados con paleta de colores coherente!")
print(paste("Accuracies:", paste(round(accuracies, 2), "%", collapse = ", ")))
print(paste("Tiempos:", paste(round(tiempos, 4), "s", collapse = ", "))) 