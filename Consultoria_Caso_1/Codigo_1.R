library(readxl)
library(nortest)
datos <- read_excel("Datos/Corregida - COMPILADO DATOS COMUNIDAD DE CUIDADO Y CENTRO DÍA.xlsx", 
                      range = "B2:AN561", col_types = c("date", 
                      "skip", "text", "text", "text", "skip", 
                      "text", "numeric", "text", "skip", 
                      "skip", "skip", "skip", "numeric", 
                      "skip", "skip", "skip", "skip", "numeric", 
                      "skip", "skip", "skip", "skip", "numeric", 
                      "skip", "skip", "skip", "skip", "numeric", 
                      "skip", "skip", "skip", "skip", "numeric", 
                      "skip", "skip", "skip", "skip", "numeric"))
colnames(datos) <- c("Fecha", "Localidad", "Programa", "Unidad_atencion", "Sexo", 
                     "Edad", "Etnia", "Peso", "Talla", "LRT_CM","LRT_A", "LRM_90","LRM_R")
datos$Sexo <- as.factor(datos$Sexo)
datos$Etnia <- as.factor(datos$Etnia)
View(datos)

#### ANÁLISIS DESCRIPTIVO #### 
summary(datos$Sexo)
summary(datos$Sexo)/length(datos$Sexo)

summary(datos$Edad)
hist(datos$Edad, freq=FALSE)
lines(density(datos$Edad), col ="red")
boxplot(datos$Edad)
shapiro.test(datos$Edad)
# No se tiene normalidad en la edad según el test de Shapiro-Wilks
ks.test(datos$Edad, pnorm)
lillie.test(datos$Edad)
# No se tiene normalidad en la edad según el test de Kolmogorov-Smirnov

summary(datos$Etnia)
summary(datos$Etnia)/length(datos$Etnia)
# No se debería incluír un factor tan sesgado en el modelo
# Se puede tener en cuenta, sim embargo

summary(datos$Peso)
hist(datos$Peso, freq = FALSE)
lines(density(datos$Peso), col = "red")
boxplot(datos$Peso)
boxplot(datos$Peso ~ datos$Sexo)
shapiro.test(datos$Peso)
# No se tiene normalidad en la edad según el test de Shapiro-Wilks
ks.test(datos$Peso, pnorm)
lillie.test(datos$Peso)
# No se tiene normalidad en la edad según el test de Kolmogorov-Smirnov

summary(datos$Talla)
hist(datos$Talla, freq = FALSE)
lines(density(datos$Talla), col = "red")
shapiro.test(datos$Talla)
box_talla <- boxplot(datos$Talla)
datos[which(datos$Talla == box_talla$out),]

boxplot(datos$Talla ~ datos$Sexo)
#alpha = 0.05
# No se tiene normalidad en la edad según el test de Shapiro-Wilks
ks.test(datos$Talla, pnorm)
lillie.test(datos$Talla)
# No se tiene normalidad en la edad según el test de Kolmogorov-Smirnov



summary(datos$LRT_CM)
hist(datos$LRT_CM, freq = FALSE)
lines(density(datos$LRT_CM), col = "red")
shapiro.test(datos$LRT_CM)
box_lrtcm <- boxplot(datos$LRT_CM)
datos[which(datos$LRT_CM == box_lrtcm$out[1]),]
datos[which(datos$LRT_CM == box_lrtcm$out[2]),]
datos[which(datos$LRT_CM == box_lrtcm$out[3]),]

boxplot(datos$LRT_CM ~ datos$Sexo)
#alpha = 0.05
# No se tiene normalidad en la edad según el test de Shapiro-Wilks
ks.test(datos$LRT_CM, pnorm)
lillie.test(datos$LRT_CM)
# No se tiene normalidad en la edad según el test de Kolmogorov-Smirnov



summary(datos$LRT_A)
hist(datos$LRT_A, freq = FALSE)
lines(density(datos$LRT_A), col = "red")
shapiro.test(datos$LRT_A)
box_lrta <- boxplot(datos$LRT_A)
datos[which(datos$LRT_A == box_lrta$out[1]),]
datos[which(datos$LRT_A == box_lrta$out[2]),]
datos[which(datos$LRT_A == box_lrta$out[3]),]
boxplot(datos$LRT_A ~ datos$Sexo)
#alpha = 0.05
# No se tiene normalidad en la edad según el test de Shapiro-Wilks
ks.test(datos$LRT_A, pnorm)
lillie.test(datos$LRT_A)
# No se tiene normalidad en la edad según el test de Kolmogorov-Smirnov


summary(datos$LRM_90)
hist(datos$LRM_90, freq = FALSE)
lines(density(datos$LRM_90), col = "red")
shapiro.test(datos$LRM_90)
box_lrm90 <- boxplot(datos$LRM_90)

datos[which(datos$LRM_90== box_lrm90$out[1]),]
datos[which(datos$LRM_90== box_lrm90$out[2]),]
datos[which(datos$LRM_90== box_lrm90$out[3]),]
datos[which(datos$LRM_90== box_lrm90$out[4]),]
datos[which(datos$LRM_90== box_lrm90$out[5]),]
datos[which(datos$LRM_90== box_lrm90$out[6]),]

boxplot(datos$LRM_90 ~ datos$Sexo)
#alpha = 0.05
# No se tiene normalidad en la edad según el test de Shapiro-Wilks
ks.test(datos$LRM_90, pnorm)
lillie.test(datos$LRM_90)
# No se tiene normalidad en la edad según el test de Kolmogorov-Smirnov

summary(datos$LRM_R)
which(datos$LRM_R == 31.45)
which(datos$LRM_R == 52.5)
datos[553,]

?summary

hist(datos$LRM_R, freq = FALSE)
lines(density(datos$LRM_R), col = "red")
shapiro.test(datos$LRM_R)
blancosm <- datos[datos$Etnia=='Blanco-Mestizo',]
shapiro.test(blancosm$LRM_R)
box_lrmr <- boxplot(datos$LRM_R)
datos[which(datos$LRM_R== box_lrmr$out[1]),]

boxplot(datos$LRM_R ~ datos$Sexo)
boxplot(datos$LRM_R ~ datos$Etnia)
#alpha = 0.05
# No se tiene normalidad en la edad según el test de Shapiro-Wilks
ks.test(datos$LRM_R, pnorm)
lillie.test(datos$LRM_R)

ks.test(blancosm$LRM_R, pnorm)
lillie.test(blancosm$LRM_R)
# No se tiene normalidad en la edad según el test de Kolmogorov-Smirnov


# Comparar CM y A
boxplot(data.frame(datos$LRT_CM, datos$LRT_A))
boxplot(data.frame(datos$LRM_90, datos$LRM_R))

boxplot(data.frame(blancosm $LRT_CM, blancosm$LRT_A))
boxplot(data.frame(blancosm $LRM_90, blancosm$LRM_R))

cor(datos[-c(1:5,7)])
# cor.test para muestras pareadas
cor.test(datos$Talla, datos$Edad) # No cor
cor.test(datos$Talla, datos$LRT_CM) # Cor
cor.test(datos$Talla, datos$Peso) # Cor
cor.test(datos$Talla, datos$LRT_A) # Cor
cor.test(datos$Talla, datos$LRM_90)# Cor
cor.test(datos$Talla, datos$LRM_R)# Cor

plot(cor(datos[-c(1:5,7)]))
pairs(datos[-c(1:5,7)], 
      main = "Matriz de Scatter Plots", 
      pch = 19,   # Puntos sólidos
      col = "blue") 

library(GGally)
# install.packages("GGally")
# Crear la matriz de gráficos con líneas de regresión
ggpairs(
  datos[-c(1:5, 7)],  # Tu subconjunto de datos
  lower = list(continuous = wrap("smooth", method = "lm", color = "red")),  # Línea de regresión en rojo
  title = "Matriz de Gráficos con Regresión Roja"
)


