# Script para generar gráficos faltantes del proyecto IRIS
library(caret)
library(randomForest)
library(rpart)
library(rpart.plot)
library(e1071)
library(kknn)
library(nnet)
library(dplyr)

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

# Generar matrices de confusión
png("matriz_confusion_knn.png", width=800, height=600)
plot(cm_knn$table, main = "Matriz de Confusión - KNN", 
     col = c("#FFE6E6", "#E6FFE6"), text.col = "black")
dev.off()

png("matriz_confusion_svm.png", width=800, height=600)
plot(cm_svm$table, main = "Matriz de Confusión - SVM", 
     col = c("#FFE6E6", "#E6FFE6"), text.col = "black")
dev.off()

png("matriz_confusion_dt.png", width=800, height=600)
plot(cm_dt$table, main = "Matriz de Confusión - Decision Tree", 
     col = c("#FFE6E6", "#E6FFE6"), text.col = "black")
dev.off()

png("matriz_confusion_nb.png", width=800, height=600)
plot(cm_nb$table, main = "Matriz de Confusión - Naive Bayes", 
     col = c("#FFE6E6", "#E6FFE6"), text.col = "black")
dev.off()

png("matriz_confusion_rf.png", width=800, height=600)
plot(cm_rf$table, main = "Matriz de Confusión - Random Forest", 
     col = c("#FFE6E6", "#E6FFE6"), text.col = "black")
dev.off()

png("matriz_confusion_nn.png", width=800, height=600)
plot(cm_nn$table, main = "Matriz de Confusión - Neural Network", 
     col = c("#FFE6E6", "#E6FFE6"), text.col = "black")
dev.off()

# Árbol de decisión
png("arbol_decision.png", width=1000, height=800)
rpart.plot(dt_model, main = "Árbol de Decisión - Dataset IRIS")
dev.off()

# Importancia de variables RF
png("importancia_variables_rf.png", width=800, height=600)
varImpPlot(rf_model, main = "Importancia de Variables - Random Forest")
dev.off()

# Gráfico comparativo de accuracy
accuracies <- c(
  cm_knn$overall["Accuracy"],
  cm_svm$overall["Accuracy"],
  cm_dt$overall["Accuracy"],
  cm_nb$overall["Accuracy"],
  cm_rf$overall["Accuracy"],
  cm_nn$overall["Accuracy"]
) * 100

png("comparacion_accuracy.png", width=1000, height=600)
modelos <- c("KNN", "SVM", "Decision Tree", "Naive Bayes", "Random Forest", "Neural Network")
barplot(accuracies, names.arg = modelos, 
        col = rainbow(6), main = "Comparación de Accuracy por Modelo",
        ylab = "Accuracy (%)", ylim = c(0, 100))
abline(h = 90, col = "red", lty = 2, lwd = 2)
text(3.5, 92, "Línea de referencia (90%)", col = "red")
dev.off()

print("¡Todos los gráficos han sido generados exitosamente!")
print(paste("Accuracies:", paste(round(accuracies, 2), "%", collapse = ", "))) 