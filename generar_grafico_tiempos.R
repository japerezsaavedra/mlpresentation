# Script para generar gráfico comparativo de tiempos
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

# Medir tiempos de entrenamiento
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

# Crear gráfico comparativo de tiempos
# Aumentar tamaño y márgenes para que no se corten las etiquetas
png("comparacion_tiempos.png", width=1200, height=700)
par(mar = c(8, 5, 4, 2) + 0.1) # margen inferior grande

tiempos <- c(knn_time, svm_time, dt_time, nb_time, rf_time, nn_time)
modelos <- c("KNN", "SVM", "Decision Tree", "Naive Bayes", "Random Forest", "Neural Network")

barplot(tiempos, names.arg = modelos, 
        col = heat.colors(6), main = "Comparación de Tiempos de Entrenamiento",
        ylab = "Tiempo (segundos)", las = 2, cex.names = 1.5)

dev.off()

print("¡Gráfico de tiempos generado exitosamente!")
print(paste("Tiempos:", paste(round(tiempos, 4), "s", collapse = ", "))) 