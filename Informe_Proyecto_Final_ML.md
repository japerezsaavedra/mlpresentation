# Proyecto Final de Machine Learning Aplicado
## Descubriendo el Mundo de las Flores con ML 🌸

---

¡Bienvenido! En este informe vamos a explorar juntos cómo la inteligencia artificial puede ayudarnos a conocer mejor las flores del famoso dataset IRIS. No necesitas ser experto en estadística: aquí te explico todo paso a paso, con ejemplos y comparaciones sencillas. ¡Vamos allá!

### 1. ¿Qué datos tenemos? 📊

El dataset IRIS contiene información de 150 flores, de tres especies diferentes. Para cada flor, medimos:
- Largo y ancho del sépalo (la parte verde que protege la flor)
- Largo y ancho del pétalo (la parte colorida)

**¿Cómo se ven los datos?**
- El largo del sépalo va de 4.3 a 7.9 cm (en promedio, 5.84 cm)
- El ancho del sépalo va de 2.0 a 4.4 cm (en promedio, 3.06 cm)
- El largo del pétalo va de 1.0 a 6.9 cm (en promedio, 3.76 cm)
- El ancho del pétalo va de 0.1 a 2.5 cm (en promedio, 1.20 cm)

**¿Y las especies?**
- Hay 50 flores de cada especie: setosa, versicolor y virginica. ¡Perfectamente balanceado!

---

### 2. ¿Qué relación hay entre las medidas? 🔗

A veces, cuando una flor tiene pétalos largos, también tiene pétalos anchos. Eso se llama "correlación". En nuestro caso:
- El largo y ancho del pétalo están muy relacionados.
- El largo del sépalo también se relaciona bastante con el largo y ancho del pétalo.
- El ancho del sépalo es un poco más independiente.

**¿Qué significa esto?**
Si conocemos el largo del pétalo, probablemente también sepamos si es ancho o no. ¡Las flores tienen sus secretos!

---

### 3. ¿Podemos resumir la información? (PCA) 🧩

A veces, hay tanta información que es mejor resumirla. Usamos una técnica llamada PCA (Análisis de Componentes Principales) que nos ayuda a ver qué medidas son las más importantes.
- Descubrimos que el largo y ancho del pétalo son las estrellas del show: explican la mayor parte de las diferencias entre flores.

---

### 4. ¿Podemos agrupar o predecir la especie de una flor? 🤖

Probamos varios modelos de Machine Learning. Aquí te cuento cómo les fue, usando ejemplos sencillos:

#### a) K-means (Agrupamiento sin saber la especie)
- Imagina que tienes que agrupar las flores solo mirando sus medidas, sin saber la especie.
- El modelo acertó solo el 26% de las veces. ¡No es fácil adivinar sin pistas!

#### b) Modelos supervisados (¡Ahora sí sabemos la especie!)
- **KNN**: Es como preguntar a los vecinos más cercanos. Acertó el 90% de las veces.
- **SVM**: Traza una línea para separar especies. Acertó el 93.33%.
- **Naive Bayes**: Usa probabilidades. Acertó el 93.33% y fue el más rápido.
- **Árbol de Decisión**: Toma decisiones paso a paso, como un juego de preguntas. Acertó el 93.33%.
- **Random Forest**: Varios árboles decidiendo juntos. Acertó el 93.33%.
- **Red Neuronal**: Imita el cerebro, aprende patrones complejos. ¡Acertó el 96.67%!

---

### 5. ¿Cuál modelo es mejor? 🏆

| Modelo           | Precisión (%) | ¿Qué tan rápido? |
|------------------|--------------|------------------|
| K-means          | 26           | Muy rápido       |
| KNN              | 90           | Lento            |
| SVM              | 93.33        | Rápido           |
| Naive Bayes      | 93.33        | ¡El más rápido!  |
| Árbol Decisión   | 93.33        | Rápido           |
| Random Forest    | 93.33        | Rápido           |
| Red Neuronal     | 96.67        | Rápido           |

- **Más preciso**: Red Neuronal
- **Más rápido**: Naive Bayes
- **Más fácil de entender**: Árbol de Decisión
- **Para explorar sin saber la especie**: K-means

---

### 6. ¿Qué aprendimos? 🤔

- ¡Las flores del dataset IRIS son bastante fáciles de clasificar para los modelos de Machine Learning!
- Si quieres la máxima precisión, usa una Red Neuronal.
- Si prefieres velocidad, Naive Bayes es tu amigo.
- Si quieres entender cómo decide el modelo, elige un Árbol de Decisión.
- Y si solo quieres explorar, K-means te puede sorprender.

---

### 7. Recomendaciones para principiantes 🌱

- No te asustes por los nombres raros: cada modelo es como una herramienta diferente para resolver el mismo problema.
- ¡Juega con los datos! Cambia parámetros, mira los gráficos, y pregúntate siempre "¿por qué?".
- Recuerda: lo importante no es solo el resultado, sino entender el camino.

---

¡Gracias por leer! Si tienes dudas, ¡pregunta! Aprender Machine Learning es como cuidar un jardín: con paciencia y curiosidad, verás florecer tu conocimiento. 🌷 