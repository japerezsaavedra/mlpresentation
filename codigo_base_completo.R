##############

#install.packages('dplyr')
#install.packages('ggplot2')
#install.packages('e1071')
#install.packages('rpart')
#install.packages('randomForest')
#install.packages('kknn')
#install.packages('caret')
#install.packages('rpart.plot')
#install.packages('PerformanceAnalytics')
#install.packages('factoextra')
#install.packages('plyr')
#install.packages("flextable")
#install.packages("nnet")


library(nnet)
library(flextable)
library(dplyr)
library(ggplot2)
library(e1071)
library(rpart)
library(randomForest)
library(kknn)
library(caret)
library(rpart.plot)
library(PerformanceAnalytics)
library(factoextra)
library(plyr)


#CORRELACION ML supervisado varibles de entrada respecto a variable de decision
bbdd1 <- iris ##Seleccionar variable - columnas, para analisis descriptivo de la variable de decision y variables de entrada
str(bbdd1)
## entrega descripcion estadistica de la variable de decision y variables de entrada
summary(bbdd1) 
#str(bbdd1)
boxplot(bbdd1,col = rainbow(ncol(trees)),las = 2, xlab = "", ylab = "") ## dibuja summary junto a outlayers para variables de entrada y variable de decision

## Pasar variable categ rica Species a num rico para realizar correlaci n
bbdd2 <- bbdd1
str(bbdd2)
bbdd2$Species <- as.numeric(bbdd2$Species) ## pasa variable categorica a n mero en columna species
str(bbdd2)
summary(bbdd2) ## entrega descripcion estadistica de la variable de decision y variables de entrada

## entrega correlacion de las variables de entrada y valiable de decision
cor(bbdd2) 
Tabla_Corr <- chart.Correlation(bbdd2,method = "spearman", plot=FALSE) ## dibuja correlacion de las variables de entrada y variable de decision


## PCA
## AJUSTE DEL MODELO
## Para ajustar el modelo lo primero que tenemos que hacer es una correlaci?n. Esto no es de utilidad para saber el comportamientos
## de las variables de la base de datos. Lo ideal es que todas las variables est?n correlacionadas. Esto significa que los valores de 
## correlaci?n sean uno o est?n cercanos a uno. Si son ceranacas a cero, pimplica que las variables las variables no se influyen unas con otras. 


## Normalizaci?n
## La normalizaci?n de las variables significa quitar las escalas y dejar que todas sean iguales. La funci?n deja las escalas de 0 a 1 para 
## todas las variables. 
## Para realizar el an?lisis de componentes principales siempre es requisito normalizar, y ademas, solo se pueden considerar variables num?ricas

norm <- function(x){(x-min(x))/(max(x)-min(x))} # Crear funci n de normalizaci n
bbdd <- select(bbdd2, 1:4)
bbdd_norm <- data.frame(apply(bbdd, 2, norm)) # aplicar funci n norm con comando apply y luego convertir en data frame o tabla con 
# comando data.frame

# summary para verificar el éxito de la normalización, don de todos los mínimos deberán ser "0" y los máximos "1"
summary(bbdd_norm)

## An?lisis de Componentes Principales
## Del an?lisis de componentes principales se obtiene un proceso llamado rotaci?n, donde se encuentra en el punto medio y se rotan los 
## datos hacia el origin. Lo importante de este procedimientos es que no se altera su comportamiento, ?nicamente 
## cambiamos el eje de coordenadas. Por otro lado, se obtienen las cargas factoriales, que son los valores de coordenadas para 
## encontrar un punto particular de la base de datos. 
## 

acp<-prcomp(bbdd_norm)


# Resultados
#
# Gr?fico de sedimentaci?n (Varianza): 
# Para ver cu?l es el resultado de cada una de la componente se puede utilizar el gr?fico de sedimentaci?n.
# El gr?fico de sedimentaci?n de la varianza nos entrega como resultado el nivel de explicaci?n que tiene cada componente respecto a la varianza.
# Cada componente puede explicar cierta cantidad de la varianza. Los comonentes que exlpiquen mejor la varianza se mantienen y los otros se descartan,
# rediciendo as? los datos. 
screeplot(acp,type="lines")

## Componentes principales (los 3 primeros)
## Con estas variables (componentes principales), se vuelve a realizar una correlaci?n con las variables originales.
cp<-data.frame(acp$x)
cp<-cp[,1:2]

#cp_distrib<-data.frame(acp$x)
#Tabla_Corr <- chart.Correlation(cp_distrib,method = "spearman", plot=FALSE) 
#Tabla_Corr <- chart.Correlation(cp_distrib,method = "pearson", plot=FALSE) 

## Correlaci?n: Constructos vs Datos Originales
## Las correlaciones nos dir?n qu? variables corresponden a un pefil o a otro perfl. As? de podr?n identificar los
## perfiles (dados por los componentes principales), y se podr? estabecer una hip?tesis de por qu? tienen este comportamiento. 
#cor(bbdd_norm, cp,use="everything", method=c("pearson"))

cor(bbdd_norm, cp,use="everything", method=c("spearman"))

#cor(bbdd_norm, cp,use="everything", method=c("kendall"))

#                 PC1         PC2
#Sepal.Length  0.9171612 -0.31201205
#Sepal.Width  -0.2907009 -0.85951510
#Petal.Length  0.9744176 -0.10108241
#Petal.Width   0.9686965 -0.09212231

## Para reajustes de modelo, se eliminará la variable Sepal.Width




# determinar numero Optimo k (cluster)

## Escalamiento BBDD para trabajar con kmean
irisScale = scale(iris[,-5])
summary(irisScale)


# Average silhouette
fviz_nbclust(irisScale, kmeans, method = "silhouette")

#fviz_nbclust(bbdd_norm, kmeans, method = "silhouette")


##MACHINE LEARNIN NO SUPERVISADO

## MODELO KMEANS

# comenzar a medir tiempo (se guarda hora de inicio)
st.time<-Sys.time()

# Creaci n del Modelo KMEANS
fitK <- kmeans(irisScale, 3) ## Genera Moldelo ML no supervisado Kmeans/Identificar clases de las especies con valores numericos para un K=3

#finalizaci n de medici n de tiempo  (se guarda hora de fin)
end.time<-Sys.time() 
# resta entre la hora de inicio y hora de fin entrenamiento
kmeansTime = end.time-st.time
# se imprime el tiempo de entrenamiento
print(kmeansTime)

iris$cluster<-fitK$cluster ## agrega la columna cluster a la BBDD IRIS
fviz_cluster(fitK, data = iris[,-c(5,6)],geom = c("point")) ## dibuja los clusters indentificados para K=3

#fviz_nbclust(irisScale, kmeans, method = "wss")
#fviz_nbclust(irisScale, kmeans, method = "gap_stat")

## PREDICCIONES / ACCURACY

iris$cluster<-factor(iris$cluster) ## tabla visual de las predicciones
iris$clus<-mapvalues(iris$cluster,c("1","2","3"),c("setosa","versicolor" ,"virginica")) ##Categoriza variables de decision str 
Indicadores_KMEANS <- confusionMatrix(iris$Species,iris$clus) ## entrega resumen del modelado, matriz de confusion y accuracy

#Indicadores_KMEANS


##MACHINE LEARNING SUPERVISADO

## Normalización BBDD1 entradas para trabajo con algoritmos
## de machine learning supervisado que necesiten que la BBDD
## esté normalizada

bbdd1$Sepal.Length <- bbdd_norm$Sepal.Length
bbdd1$Sepal.Width <- bbdd_norm$Sepal.Width
bbdd1$Petal.Length <- bbdd_norm$Petal.Length
bbdd1$Petal.Width <- bbdd_norm$Petal.Width

##Entrenamiento y prueba 
set.seed(123)
training.samples <- bbdd1$Species %>% 
  createDataPartition(p = 0.8, list = FALSE)
ta.train <- bbdd1[training.samples, ]
ta.test <- bbdd1[-training.samples, ]

#summary(ta.test$Species)
#summary(ta.train$Species)

####### Modelo KNN ######

# comenzar a medir tiempo (se guarda hora de inicio)
st.time<-Sys.time()

##Modelo KNN con datos de entrenamiento de tasa de aprobacion
knn_ta<-train.kknn(Species ~ .,data=ta.train,kmax=10)

end.time<-Sys.time() 
# resta entre la hora de inicio y hora de fin entrenamiento
knnTime = end.time-st.time
# se imprime el tiempo de entrenamiento
print(knnTime)

##Prediccion del modelo KNN con datos de prueba de tasa de aprobacion
pred_knn_ta <-predict(knn_ta, newdata = ta.test)
#pred_knn_ta

##Matriz de confusion del modelo KNN con datos de validacion tasa de aprobacion
Indicadores_KNN<-confusionMatrix(ta.test$Species,pred_knn_ta)
#Indicadores_KNN


####### Modelo SVM ######

# comenzar a medir tiempo (se guarda hora de inicio)
st.time<-Sys.time()

##Modelo SVM con datos de entrenamiento de tasa de aprobacion
svm_ta<-svm(Species~.,data=ta.train,kernel="radial",type = "C-classification")

#finalizaci n de medici n de tiempo  (se guarda hora de fin)
end.time<-Sys.time() 
# resta entre la hora de inicio y hora de fin entrenamiento
svmTime = end.time-st.time
# se imprime el tiempo de entrenamiento
print(svmTime)


##Prediccion del modelo SVM con datos de prueba de tasa de aprobacion
pred_svm_ta <-predict(svm_ta, newdata = ta.test)

##Matriz de confusion del modelo SVM con datos de validacion tasa de aprobacion
Indicadores_SVM<-confusionMatrix(ta.test$Species,pred_svm_ta)



####### Modelo DT ######

# comenzar a medir tiempo (se guarda hora de inicio)
st.time<-Sys.time()

##Modelo DT con datos de entrenamiento de tasa de aprobacion
dt_ta <- rpart(Species ~., data = ta.train)

#finalizaci n de medici n de tiempo  (se guarda hora de fin)
end.time<-Sys.time() 
# resta entre la hora de inicio y hora de fin entrenamiento
dtTime = end.time-st.time
# se imprime el tiempo de entrenamiento
print(dtTime)

##Prediccion del modelo DT con datos de prueba de tasa de aprobacion
pred_dt_ta <-predict(dt_ta, newdata = ta.test, type = 'class')


##Matriz de confusion del modelo DT con datos de validacion tasa de aprobacion
Indicadores_DT<-confusionMatrix(ta.test$Species,pred_dt_ta)



####### Modelo NB ######

# comenzar a medir tiempo (se guarda hora de inicio)
st.time<-Sys.time()

##Modelo NB con datos de entrenamiento de tasa de aprobacion
nb_ta<-naiveBayes(Species  ~ ., data = ta.train)

#finalizaci n de medici n de tiempo  (se guarda hora de fin)
end.time<-Sys.time() 
# resta entre la hora de inicio y hora de fin entrenamiento
nbTime = end.time-st.time
# se imprime el tiempo de entrenamiento
print(nbTime)

##Prediccion del modelo NB con datos de prueba de tasa de aprobacion
pred_nb_ta <-predict(nb_ta, newdata = ta.test)


##Matriz de confusion del modelo NB con datos de validacion tasa de aprobacion
Indicadores_NB<-confusionMatrix(ta.test$Species,pred_nb_ta)




####### Modelo RF ######

# comenzar a medir tiempo (se guarda hora de inicio)
st.time<-Sys.time()

##Modelo RF con datos de entrenamiento de tasa de aprobacion
rf_ta<-randomForest(Species ~ .,data = ta.train,ntree = 250, na.action = na.roughfix)

#finalizaci n de medici n de tiempo  (se guarda hora de fin)
end.time<-Sys.time() 
# resta entre la hora de inicio y hora de fin entrenamiento
rfTime = end.time-st.time
# se imprime el tiempo de entrenamiento
print(rfTime)

##Prediccion del modelo RF con datos de prueba de tasa de aprobacion
pred_rf_ta <-predict(rf_ta, newdata = ta.test)
pred_rf_ta

##Matriz de confusion del modelo RF con datos de validacion tasa de aprobacion
Indicadores_RF<-confusionMatrix(ta.test$Species,pred_rf_ta)




####### Modelo NN ######

# comenzar a medir tiempo (se guarda hora de inicio)
st.time<-Sys.time()

##Modelo NN con datos de entrenamiento de tasa de aprobacion
nn_ta <- nnet(Species ~ ., data = ta.train, size = 5, decay = 5e-4, maxit = 200, trace  = FALSE)


##size = 5
#Número de neuronas en la capa oculta.
#Cuanto mayor sea, más capacidad de ajuste (pero también más riesgo de sobreajuste y más tiempo de cómputo).
#Habitualmente se prueba con varios valores para encontrar el óptimo.
##Por qué 5:
#Permite capturar interacciones no lineales moderadas sin sobreajustar inmediatamente.
#Es un valor inicial común cuando el número de predictores no es muy grande (por regla empírica, tamaño oculto ≈ número de predictores/2).


##decay = 5e-4
#Tasa de regularización (weight decay).
#Penaliza pesos de gran magnitud para evitar sobreajuste.
#Valores típicos van desde 0 (sin regularización) hasta, por ejemplo, 1e‑2.
##Por qué 5e-4:
#Valor pequeño que introduce ligera regularización, suficiente para estabilizar el entrenamiento sin anular la capacidad de la red.
#Basado en recomendaciones de la literatura y en pruebas previas: valores típicos suelen estar entre 1e‑4 y 1e‑2, y 5e‑4 es un buen punto de partida intermedio.


##maxit = 200
#Número máximo de iteraciones (épocas) del algoritmo de optimización.
#Si el entrenamiento no converge antes, se detiene al alcanzar este número de iteraciones.
##Por qué 200:
#Suele ser suficiente para que converja en problemas de tamaño y complejidad moderada.
#Evita entrenamientos excesivamente largos si el modelo no mejora ya el error después de cierto número de pasos.


##trace = FALSE
#Control de salida por pantalla durante el entrenamiento.
#TRUE: muestra en cada iteración la evolución del valor de la función de pérdida.
#FALSE: entrena "en silencio" y solo muestra resultados al final.
##Por qué FALSE:
#Permite un log limpio cuando se integrará este bloque en scripts o pipelines automatizados.
#Si necesitas depurar convergencia, podrías ponerlo en TRUE, pero solo en etapas de diagnóstico.

#finalizaci n de medici n de tiempo  (se guarda hora de fin)
end.time<-Sys.time() 
# resta entre la hora de inicio y hora de fin entrenamiento
nnTime = end.time-st.time
# se imprime el tiempo de entrenamiento
print(nnTime)

##Prediccion del modelo DT con datos de prueba de tasa de aprobacion
pred_nn_ta <-predict(nn_ta, newdata = ta.test, type = 'class')
#pred_nn_ta
pred_nn_ta <- factor(pred_nn_ta)

##Matriz de confusion del modelo DT con datos de validacion tasa de aprobacion
Indicadores_NN<-confusionMatrix(ta.test$Species,pred_nn_ta)



# COMPARACION ACCURACY (Eficacia)

Resumen<-data.frame(KMEANS=Indicadores_KMEANS$overall,SVM=Indicadores_SVM$overall,KNN=Indicadores_KNN$overall,
                    NB=Indicadores_NB$overall,DT=Indicadores_DT$overall,RF=Indicadores_RF$overall,NN=Indicadores_NN$overall)
#Resumen
round(Resumen*100,2)

# COMPARACI N EFICIENCIA
# Crear un vector con los tiempos (KEANS no tiene tiempo asociado aqu , por lo que se usa NA)
tiempos <- c(kmeansTime/100, svmTime/100, knnTime/100, nbTime/100, dtTime/100, rfTime/100, nnTime/100)
#tiempos

# Agregar la fila de "Tiempo" al data frame
Resumen_time <- rbind(Resumen, Tiempo = tiempos)
round(Resumen_time*100,5) 