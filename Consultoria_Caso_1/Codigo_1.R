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
boxplot(datos$Talla)
boxplot(datos$Talla ~ datos$Sexo)
#alpha = 0.05
# No se tiene normalidad en la edad según el test de Shapiro-Wilks
ks.test(datos$Talla, pnorm)
lillie.test(datos$Talla)
# No se tiene normalidad en la edad según el test de Kolmogorov-Smirnov
