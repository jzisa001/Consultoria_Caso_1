# Librerias 
library(readxl)
library(nortest)
library(dplyr)
library(ggplot2)
library(GGally) # para la funcion ggpairs

# Lectura base de datos
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

# Análisis descriptivo #### 

## Sexo ####
summary(datos$Sexo)
summary(datos$Sexo)/length(datos$Sexo)

datos_resumen <- datos %>%
  dplyr::count(Sexo) %>%
  dplyr::mutate(porcentaje = n / sum(n) * 100)

ggplot(datos_resumen, aes(x = "", y = porcentaje, fill = Sexo)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  theme_void() +  # Elimina fondo y ejes
  labs(title = "Porcentaje de Hombres y Mujeres", fill = "Sexo") +
  geom_text(aes(label = paste0(round(porcentaje, 1), "%")),
            position = position_stack(vjust = 0.5), color = "white")
## Edad ####
summary(datos$Edad)
hist(datos$Edad, freq=FALSE)
lines(density(datos$Edad), col ="red")
boxplot(datos$Edad)
which(datos$Edad>91)
datos[329,]
datos[519,]
shapiro.test(datos$Edad) # No se tiene normalidad en la edad según el test de Shapiro-Wilks
ks.test(datos$Edad, pnorm) # No se tiene normalidad en la edad según el test de Kolmogorov-Smirnov
lillie.test(datos$Edad) # No se tiene normalidad en la edad según el test de lillie


## Etnia ####
summary(datos$Etnia)
summary(datos$Etnia)/length(datos$Etnia)
# No se debería incluír un factor tan sesgado en el modelo
# Se puede tener en cuenta, sim embargo

## Peso ####
summary(datos$Peso)
hist(datos$Peso, freq = FALSE)
lines(density(datos$Peso), col = "red")

box_peso <- boxplot(datos$Peso)
box_peso$out
box_peso_fm <- boxplot(datos$Peso ~ datos$Sexo)
box_peso_fm$out

shapiro.test(datos$Peso) # No se tiene normalidad en la edad según el test de Shapiro-Wilks
ks.test(datos$Peso, pnorm) # No se tiene normalidad en la edad según el test de Kolmogorov-Smirnov
lillie.test(datos$Peso) # No se tiene normalidad en la edad según el test de Lilliefors


## Talla ####
summary(datos$Talla)

hist(datos$Talla, freq = FALSE)
lines(density(datos$Talla), col = "red")

box_talla <- boxplot(datos$Talla)
datos[which(datos$Talla == box_talla$out),]
box_talla_fm <- boxplot(datos$Talla ~ datos$Sexo)
box_talla_fm$out

shapiro.test(datos$Talla) # No se tiene normalidad en la edad según el test de Shapiro-Wilks
ks.test(datos$Talla, pnorm) # No se tiene normalidad en la edad según el test de Kolmogorov-Smirnov
lillie.test(datos$Talla) # No se tiene normalidad en la edad según el test de Lilliefors


## LRT_CM ####
summary(datos$LRT_CM)

hist(datos$LRT_CM, freq = FALSE)
lines(density(datos$LRT_CM), col = "red")

box_lrtcm <- boxplot(datos$LRT_CM)
datos[which(datos$LRT_CM == box_lrtcm$out[1]),]
datos[which(datos$LRT_CM == box_lrtcm$out[2]),]
datos[which(datos$LRT_CM == box_lrtcm$out[3]),]

box_lrtcm_fm <- boxplot(datos$LRT_CM ~ datos$Sexo)
box_lrtcm_fm$out

shapiro.test(datos$LRT_CM) # No se tiene normalidad en la edad según el test de Shapiro-Wilks
ks.test(datos$LRT_CM, pnorm) # No se tiene normalidad en la edad según el test de Kolmogorov-Smirnov
lillie.test(datos$LRT_CM) # No se tiene normalidad en la edad según el test de Lilliefors


## LRT_A ####
summary(datos$LRT_A)

hist(datos$LRT_A, freq = FALSE)
lines(density(datos$LRT_A), col = "red")

box_lrta <- boxplot(datos$LRT_A)
datos[which(datos$LRT_A == box_lrta$out[1]),]
datos[which(datos$LRT_A == box_lrta$out[2]),]
datos[which(datos$LRT_A == box_lrta$out[3]),]

box_lrta_fm <- boxplot(datos$LRT_A ~ datos$Sexo)
box_lrta_fm$out

shapiro.test(datos$LRT_A) # No se tiene normalidad en la edad según el test de Shapiro-Wilks
ks.test(datos$LRT_A, pnorm) # No se tiene normalidad en la edad según el test de Kolmogorov-Smirnov
lillie.test(datos$LRT_A) # No se tiene normalidad en la edad según el test de Lilliefors



## LRM_90 ####
summary(datos$LRM_90)

hist(datos$LRM_90, freq = FALSE)
lines(density(datos$LRM_90), col = "red")

box_lrm90 <- boxplot(datos$LRM_90)

datos[which(datos$LRM_90== box_lrm90$out[1]),]
datos[which(datos$LRM_90== box_lrm90$out[2]),]
datos[which(datos$LRM_90== box_lrm90$out[3]),]
datos[which(datos$LRM_90== box_lrm90$out[4]),]
datos[which(datos$LRM_90== box_lrm90$out[5]),]
datos[which(datos$LRM_90== box_lrm90$out[6]),]

box_lrm90_fm <- boxplot(datos$LRM_90 ~ datos$Sexo)
box_lrm90_fm$out

shapiro.test(datos$LRM_90) # No se tiene normalidad en la edad según el test de Shapiro-Wilks
ks.test(datos$LRM_90, pnorm) # No se tiene normalidad en la edad según el test de Kolmogorov-Smirnov
lillie.test(datos$LRM_90) # No se tiene normalidad en la edad según el test de Lilliefors



## LRM_R ####
summary(datos$LRM_R)

which(datos$LRM_R == 31.45)
which(datos$LRM_R == 52.5)
datos[553,]

hist(datos$LRM_R, freq = FALSE)
lines(density(datos$LRM_R), col = "red")

box_lrmr <- boxplot(datos$LRM_R)
datos[which(datos$LRM_R== box_lrmr$out[1]),]

box_lrmr_fm <- boxplot(datos$LRM_R ~ datos$Sexo)
box_lrmr_fm$out

# boxplot(datos$LRM_R ~ datos$Etnia)

shapiro.test(datos$LRM_R) # No se tiene normalidad en la edad según el test de Shapiro-Wilks
ks.test(datos$LRM_R, pnorm) # No se tiene normalidad en la edad según el test de Kolmogorov-Smirnov
lillie.test(datos$LRM_R) # No se tiene normalidad en la edad según el test de Lilliefors

blancosm <- datos[datos$Etnia=='Blanco-Mestizo',]
shapiro.test(blancosm$LRM_R)
ks.test(blancosm$LRM_R, pnorm)
lillie.test(blancosm$LRM_R)


# Descriptivo hipótesis ####

# Comparar LRT CM y A
boxplot(data.frame(datos$LRT_CM, datos$LRT_A))

boxplot(data.frame(blancosm $LRT_CM, blancosm$LRT_A))

# Comparar LRM 90 y R
boxplot(data.frame(datos$LRM_90, datos$LRM_R))

boxplot(data.frame(blancosm $LRM_90, blancosm$LRM_R))


# Correlaciones ####
cor(datos[-c(1:5,7)])
# cor.test para muestras pareadas
cor.test(datos$Talla, datos$Edad) # No cor
cor.test(datos$Talla, datos$LRT_CM) # Cor
cor.test(datos$Talla, datos$Peso) # Cor
cor.test(datos$Talla, datos$LRT_A) # Cor
cor.test(datos$Talla, datos$LRM_90)# Cor
cor.test(datos$Talla, datos$LRM_R)# Cor

# pairs(datos[-c(1:5,7)], 
#       main = "Matriz de Scatter Plots", 
#       pch = 19,   # Puntos sólidos
#       col = "blue") 


# Crear la matriz de gráficos con líneas de regresión
ggpairs(
  datos[-c(1:5, 7)],  # Tu subconjunto de datos
  lower = list(continuous = wrap("smooth", method = "lm", color = "red")),  # Línea de regresión en rojo
  title = "Matriz de Gráficos con Regresión Roja"
)

aaaa
