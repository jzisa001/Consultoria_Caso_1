# Librerias ####
library(readxl)
library(tidyverse)
library(nortest)
library(dplyr)
library(ggplot2)
library(GGally) # para la funcion ggpairs
library(gt)
library(gtExtras)
library(moments)
library(leaps)
library(MASS)
library(lmtest)
library(car)
library(BSDA) # Test del signo
library(glmnet)
library(glmtoolbox)
library(MASS)
library(rsample)
library(lawstat)
library(symmetry)

# Lectura de datos ####
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
colnames(datos) <- c("Fecha", "Localidad", "Programa", "Unidad_atencion", 
                     "Sexo", "Edad", "Etnia", "Peso", "Talla",
                     "LRT_CM","LRT_A", "LRM_90","LRM_R")
datos$Sexo <- as.factor(datos$Sexo)
datos$Etnia <- as.factor(datos$Etnia)
datos <- datos[-which(datos$Edad<60),] # >60
datos <- datos |> filter(!(abs(LRT_A-LRT_CM) > 3 | abs(LRM_90-LRM_R) > 3))
datos <- datos |> as.data.frame()
head(datos) |> gt() |> gtExtras::gt_theme_538() |> tab_options(table.font.size = 9)

# Analisis descriptivo ####

datos_resumen <- datos %>%
  dplyr::count(Sexo) %>%
  dplyr::mutate(porcentaje = round(n / sum(n) * 100, digits=3))
datos_resumen |> gt() |> gtExtras::gt_theme_538()

ggplot(datos_resumen, aes(x = "", y = porcentaje, fill = Sexo)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(title = "Porcentaje de Hombres y Mujeres", fill = "Sexo") +
  geom_text(aes(label = paste0(round(porcentaje, 1), "%")),
            position = position_stack(vjust = 0.5), color = "white")

# Analisis de la variable edad
summary(datos$Edad)

hist(datos$Edad, freq=FALSE, main = 'Histograma de Edad', xlab = 'Edad', ylab = 'Densidad')
lines(density(datos$Edad), col ="red")
box_Edad <- boxplot(datos$Edad, main = "Boxplot de Edad")

datos[which(datos$Edad>box_Edad$stats[5] | datos$Edad<box_Edad$stats[1]),-c(1,2)] |> 
  gt() |> gtExtras::gt_theme_538() |> 
  tab_options(table.font.size = 9)

# Analisis de la variable etnia

datos |> 
  dplyr::count(Etnia) |> 
  dplyr::mutate(porcentaje = n / sum(n)) |> 
  gt() |> gtExtras::gt_theme_538() |> fmt_percent(column = 3, decimals = 3, dec_mark = ".")

# Analisis de la variable peso

summary(datos$Peso)

hist(datos$Peso, freq = FALSE, main = 'Histograma del peso', xlab = 'Peso', ylab = 'Densidad')
lines(density(datos$Peso), col = "red")
box_peso <- boxplot(datos$Peso, main = "Boxplot de Peso")

datos[which(datos$Peso>box_peso$stats[5] | datos$Peso<box_peso$stats[1]),-c(1,2)] |> gt() |> gtExtras::gt_theme_538() |> tab_options(table.font.size = 9)

# Analisis de la variable sexo

box_peso_fm <- boxplot(datos$Peso ~ datos$Sexo, xlab = 'Sexo', ylab = 'Peso', 
                       main = "Boxplot de Peso por Sexo")

datos |> filter(Sexo == "Femenino") |> 
  filter(Peso > box_peso_fm$stats[5,1] | Peso < box_peso_fm$stats[1,1]) |> 
  dplyr::select(-1,-2) |> 
  gt() |>
  gtExtras::gt_theme_538() |>
  tab_options(table.font.size = 9)

datos |> filter(Sexo == "Masculino") |> 
  filter(Peso > box_peso_fm$stats[5,2] | Peso < box_peso_fm$stats[1,2]) |> 
  dplyr::select(-1,-2) |> 
  gt() |>
  gtExtras::gt_theme_538() |>
  tab_options(table.font.size = 9)

# Analisis de la variable talla

summary(datos$Talla)

hist(datos$Talla, freq = FALSE, main = 'Histograma de Talla', xlab= 'Talla', ylab = 'Densidad')
lines(density(datos$Talla), col = "red")
box_talla <- boxplot(datos$Talla, main = "Boxplot de Talla")

datos[which(datos$Talla<box_talla$stats[1]|datos$Talla>box_talla$stats[5]),-c(1,2)] |> gt() |> gtExtras::gt_theme_538() |> tab_options(table.font.size = 9)

# Distinguiendo por sexo
box_talla_fm <- boxplot(datos$Talla ~ datos$Sexo, xlab='Sexo', ylab='Talla',
                        main = "Boxplot de Talla por Sexo")

(datos |> filter(Sexo == "Femenino"))[which((datos |> filter(Sexo == "Femenino"))$Talla<box_talla_fm$stats[1,1]|(datos |> filter(Sexo == "Femenino"))$Talla>box_talla_fm$stats[5,1]),-c(1,2)] |> gt() |> gtExtras::gt_theme_538() |> tab_options(table.font.size = 9)

(datos |> filter(Sexo == "Masculino"))[which((datos |> filter(Sexo == "Masculino"))$Talla<box_talla_fm$stats[1,2]|(datos |> filter(Sexo == "Masculino"))$Talla>box_talla_fm$stats[5,2]),-c(1,2)] |> gt() |> gtExtras::gt_theme_538() |> tab_options(table.font.size = 9)

# Analisis de la variable LRT_CM

summary(datos$LRT_CM)

hist(datos$LRT_CM, freq = FALSE, main = 'Histograma de LRT medido con cinta métrica', xlab = 'Longitud en cm', ylab = 'Densidad')
lines(density(datos$LRT_CM), col = "red")
box_LRT_CM <- boxplot(datos$LRT_CM, main = "Boxplot de LRT_CM")

datos[which(datos$LRT_CM<box_LRT_CM$stats[1]|datos$LRT_CM>box_LRT_CM$stats[5]),-c(1,2)] |> gt() |> gtExtras::gt_theme_538() |> tab_options(table.font.size = 9)

# Distinguiendo por sexo

box_LRT_CM_fm <- boxplot(datos$LRT_CM ~ datos$Sexo, xlab='Sexo', ylab='Longitud en cm',
                         main = "Boxplot de LTR_CM por Sexo")

(datos |> filter(Sexo == "Femenino"))[which((datos |> filter(Sexo == "Femenino"))$LRT_CM<box_LRT_CM_fm$stats[1,1]|(datos |> filter(Sexo == "Femenino"))$LRT_CM>box_LRT_CM_fm$stats[5,1]),-c(1,2)] |> gt() |> gtExtras::gt_theme_538() |> tab_options(table.font.size = 9)

(datos |> filter(Sexo == "Masculino"))[which((datos |> filter(Sexo == "Masculino"))$LRT_CM<box_LRT_CM_fm$stats[1,2]|(datos |> filter(Sexo == "Masculino"))$LRT_CM>box_LRT_CM_fm$stats[5,2]),-c(1,2)] |> gt() |> gtExtras::gt_theme_538() |> tab_options(table.font.size = 9)

# Analisis de la variable LRT_A

summary(datos$LRT_A)

hist(datos$LRT_A, freq = FALSE, main ='Histograma de LRT con antropómetro', xlab = 'Longitud en cm', ylab = 'Densidad')
lines(density(datos$LRT_A), col = "red")
box_LRT_A <- boxplot(datos$LRT_A, main = "Boxplot de LRT_A")

datos[which(datos$LRT_A<box_LRT_A$stats[1]|datos$LRT_A>box_LRT_A$stats[5]),-c(1,2)] |> gt() |> gtExtras::gt_theme_538() |> tab_options(table.font.size = 9)

# Distinguiendo por sexo

box_LRT_A_fm <- boxplot(datos$LRT_A ~ datos$Sexo, xlab = 'Sexo', ylab = 'LRT con antropómetro en cm',
                        main = "Boxplot de LRT_A por Sexo")

(datos |> filter(Sexo == "Femenino"))[which((datos |> filter(Sexo == "Femenino"))$LRT_A<box_LRT_A_fm$stats[1,1]|(datos |> filter(Sexo == "Femenino"))$LRT_A>box_LRT_A_fm$stats[5,1]),-c(1,2)] |> gt() |> gtExtras::gt_theme_538() |> tab_options(table.font.size = 9)

(datos |> filter(Sexo == "Masculino"))[which((datos |> filter(Sexo == "Masculino"))$LRT_A<box_LRT_A_fm$stats[1,2]|(datos |> filter(Sexo == "Masculino"))$LRT_A>box_LRT_A_fm$stats[5,2]),-c(1,2)] |> gt() |> gtExtras::gt_theme_538() |> tab_options(table.font.size = 9)

# Analisis de la variable LRM_90

summary(datos$LRM_90)

hist(datos$LRM_90, freq = FALSE, main = 'Histograma de LRM a 90°', xlab = 'Longitud en cm', ylab = 'Densidad')
lines(density(datos$LRM_90), col = "red")
box_LRM_90<- boxplot(datos$LRM_90, main = "Boxplot de LRM_90")

datos[which(datos$LRM_90<box_LRM_90$stats[1]|datos$LRM_90>box_LRM_90$stats[5]),-c(1,2)] |> gt() |> gtExtras::gt_theme_538() |> tab_options(table.font.size = 9)

# Distinguiendo por sexo

box_LRM_90_fm <- boxplot(datos$LRM_90 ~ datos$Sexo, xlab = 'Sexo', ylab = 'LRM a 90° en cm',
                         main = "Boxplot de LRM_90 por Sexo")

(datos |> filter(Sexo == "Femenino"))[which((datos |> filter(Sexo == "Femenino"))$LRM_90<box_LRM_90_fm$stats[1,1]|(datos |> filter(Sexo == "Femenino"))$LRM_90>box_LRM_90_fm$stats[5,1]),-c(1,2)] |> gt() |> gtExtras::gt_theme_538() |> tab_options(table.font.size = 9)

(datos |> filter(Sexo == "Masculino"))[which((datos |> filter(Sexo == "Masculino"))$LRM_90<box_LRM_90_fm$stats[1,2]|(datos |> filter(Sexo == "Masculino"))$LRM_90>box_LRM_90_fm$stats[5,2]),-c(1,2)] |> gt() |> gtExtras::gt_theme_538() |> tab_options(table.font.size = 9)

# Analisis de la variable LRM_R

summary(datos$LRM_R)

hist(datos$LRM_R, freq = FALSE, main = 'Histograma de LRM recta', xlab = 'Longitud en cm', ylab='Densidad')
lines(density(datos$LRM_R), col = "red")
box_LRM_R<- boxplot(datos$LRM_R, main = "Boxplot de LRM_R")

datos[which(datos$LRM_R<box_LRM_R$stats[1]|datos$LRM_R>box_LRM_R$stats[5]),-c(1,2)] |> gt() |> gtExtras::gt_theme_538() |> tab_options(table.font.size = 9)

# Distinguiendo por sexo

box_LRM_R_fm <- boxplot(datos$LRM_R ~ datos$Sexo, xlab = 'Sexo', ylab = 'LRM recta en cm',
                        main = "Boxplot de LRT_R por Sexo")

(datos |> filter(Sexo == "Femenino"))[which((datos |> filter(Sexo == "Femenino"))$LRM_R<box_LRM_R_fm$stats[1,1]|(datos |> filter(Sexo == "Femenino"))$LRM_R>box_LRM_R_fm$stats[5,1]),-c(1,2)] |> gt() |> gtExtras::gt_theme_538() |> tab_options(table.font.size = 9)

(datos |> filter(Sexo == "Masculino"))[which((datos |> filter(Sexo == "Masculino"))$LRM_R<box_LRM_R_fm$stats[1,2]|(datos |> filter(Sexo == "Masculino"))$LRM_R>box_LRM_R_fm$stats[5,2]),-c(1,2)] |> gt() |> gtExtras::gt_theme_538() |> tab_options(table.font.size = 9)

# Relaciones entre variables ####

ggpairs(
  datos[,-c(1:5, 7)],
  lower = list(continuous = wrap("smooth", method = "lm", color = "lightblue")),
  title = "Matriz de correlaciones"
)

# Talla - variables cualitativas

ggplot(datos, aes(x = Sexo, y = Talla)) +
  geom_boxplot() +
  labs(title = "Distribución de la Talla por Sexo",
       x = "Sexo",
       y = "Talla") +
  theme_minimal()

ggplot(datos, aes(x = Etnia, y = Talla)) +
  geom_boxplot() +
  labs(title = "Distribución de la Talla por Etnia",
       x = "Etnia",
       y = "Talla") +
  theme_minimal()

# Hipotesis ####

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
colnames(datos) <- c("Fecha", "Localidad", "Programa", "Unidad_atencion", 
                     "Sexo", "Edad", "Etnia", "Peso", "Talla",
                     "LRT_CM","LRT_A", "LRM_90","LRM_R")
datos$Sexo <- as.factor(datos$Sexo)
datos$Etnia <- as.factor(datos$Etnia)
datos <- datos[-which(datos$Edad<60),] # >60
datos <- datos |> filter(!(abs(LRT_A-LRT_CM) > 3 | abs(LRM_90-LRM_R) > 3))
datos <- datos |> as.data.frame()

# Longitud rodilla talon

boxplot(data.frame(datos$LRT_CM, datos$LRT_A), ylab = 'Longitud Rodilla - Talón (cm)', xgap.axis = c("Cinta métrica","Antropómetro"), xaxt = "n",
        main = "Boxplot Longitud rodilla - talón por tipo de medición")
axis(1, at = c(1, 2), labels = c("Cinta Métrica", "Antropómetro"))

LRT_dif <- datos$LRT_CM - datos$LRT_A

hist(LRT_dif, freq = FALSE, main = 'Histograma de las la diferencias LRT_CM - LRT_A', ylab = 'Densidad', xlab = 'Diferencia (cm)')
lines(density(LRT_dif), col = "red")
abline(v = mean(LRT_dif, na.rm = TRUE), col = "red", lty = 2)
boxplot_dif <- boxplot(LRT_dif, main = "Boxplot de las diferencias LRT_CM - LRT_A")

qqnorm(LRT_dif, main = "Q-Q plot de las diferencias")
qqline(LRT_dif, col = "red")

# Test Lilliefors

lillie.test(LRT_dif) 

# Simetria

set.seed(123)
symmetry_test(LRT_dif, "MI", mu = mean(LRT_dif))

# Transformacion logaritmica

lLRT_A <- log(datos$LRT_A)
lLRT_CM <- log(datos$LRT_CM)

lLRT_dif <- lLRT_CM - lLRT_A

hist(lLRT_dif, freq = FALSE, main = 'Histograma de las la diferencias LRT_CM - LRT_A', ylab = 'Densidad', xlab = 'Diferencia (cm)')
lines(density(lLRT_dif), col = "red")
abline(v = mean(lLRT_dif, na.rm = TRUE), col = "red", lty = 2)
boxplot_dif <- boxplot(lLRT_dif, main = "Boxplot de las diferencias ln(LRT_CM) - ln(LRT_A)")

# Test Lilliefors

lillie.test(lLRT_dif) 

# Simetria

set.seed(123)
symmetry_test(lLRT_dif, "MI", mu = mean(lLRT_dif))

# Test de Wilcoxon

wilcox.test(lLRT_CM, lLRT_A, paired = TRUE, alternative = "two.sided", conf.int = TRUE)

# Distinguiendo por sexo

# Masculino

hombres <- datos[datos$Sexo == "Masculino",]
LRT_dif <- hombres$LRT_CM - hombres$LRT_A

hist(LRT_dif, freq = FALSE, main = 'Diferencia de mediciones LRT (hombres)', ylab = 'Densidad', xlab = 'Diferenencia (cm)')
lines(density(LRT_dif), col = "red")
abline(v = median(LRT_dif, na.rm = TRUE), col = "red", lty = 2)
boxplot_dif <- boxplot(LRT_dif, main = "Boxplot de las diferencias LRT sexo masculino")

# Test Lilliefors

lillie.test(LRT_dif)

# Simetria

set.seed(123)
symmetry_test(LRT_dif, "MI", mu = mean(LRT_dif))

# Transformacion logaritmica

lLRT_A <- log(hombres$LRT_A)
lLRT_CM <- log(hombres$LRT_CM)}

lLRT_dif <- lLRT_CM - lLRT_A

hist(lLRT_dif, freq = FALSE, main = 'Histograma de las la diferencias LRT_CM - LRT_A', ylab = 'Densidad', xlab = 'Diferencia (cm)')
lines(density(lLRT_dif), col = "red")
abline(v = mean(lLRT_dif, na.rm = TRUE), col = "red", lty = 2)
boxplot_dif <- boxplot(lLRT_dif, main = "Boxplot de las diferencias ln(LRT) sexo masculino")

# Test Lilliefors

lillie.test(lLRT_dif) 

# Simetria

set.seed(123)
symmetry_test(lLRT_dif, "MI", mu = mean(lLRT_dif))

# Test de Wilcoxon

wilcox.test(lLRT_CM, lLRT_A, paired = TRUE, alternative = "two.sided", conf.int = TRUE)

# Femenino

mujeres <- datos[datos$Sexo == "Femenino",]
LRT_dif <- mujeres$LRT_CM - mujeres$LRT_A

# Como se ve graficamente, parece que no hay normalidad
hist(LRT_dif, freq = FALSE, main = 'Diferencia de mediciones LRT (mujeres)', ylab = 'Densidad', xlab = 'Diferenencia (cm)')
lines(density(LRT_dif), col = "red")
abline(v = median(LRT_dif, na.rm = TRUE), col = "red", lty = 2)
boxplot_dif <- boxplot(LRT_dif, main = "Boxplot diferencia LRT sexo femenino") # hay muchos outliers

# Test Lilliefors

lillie.test(LRT_dif)

# Simetria

set.seed(123)
symmetry_test(LRT_dif, "MI", mu = mean(LRT_dif))

# Test de Wilcoxon

wilcox.test(mujeres$LRT_CM, mujeres$LRT_A, paired = TRUE, alternative = "two.sided", conf.int = TRUE)

# Analisis por etnia

# Diferencias CM y A 
bm <- datos[datos$Etnia=="Blanco-Mestizo",]
LRT_dif <- bm$LRT_CM - bm$LRT_A

# Como se ve graficamente, parece que no hay normalidad
hist(LRT_dif, freq = FALSE, main = 'Histograma de las la diferencia de mediciones LRT', ylab = 'Densidad', xlab = 'Diferenencia (cm)')
lines(density(LRT_dif), col = "red")
abline(v = median(LRT_dif, na.rm = TRUE), col = "red", lty = 2)
boxplot_dif <- boxplot(LRT_dif, main = "Boxplot diferencias LRT etnia Blanco-Mestizo") # hay muchos outliers

# Test Lilliefors

lillie.test(LRT_dif)

# Simetria

set.seed(123)
symmetry_test(LRT_dif, "MI", mu = mean(LRT_dif))

# Transformacion logaritmica

lLRT_A <- log(bm$LRT_A)
lLRT_CM <- log(bm$LRT_CM)

lLRT_dif <- lLRT_CM - lLRT_A

hist(lLRT_dif, freq = FALSE, main = 'Histograma de las la diferencias LRT_CM - LRT_A', ylab = 'Densidad', xlab = 'Diferencia (cm)')
lines(density(lLRT_dif), col = "red")
abline(v = mean(lLRT_dif, na.rm = TRUE), col = "red", lty = 2)
boxplot_dif <- boxplot(lLRT_dif, main = "Boxplot de las diferencias ln(LRT) etnia Blanco-Mestizo")

# Test Lilliefors

lillie.test(lLRT_dif) 

# Simetria

set.seed(123)
symmetry_test(lLRT_dif, "MI", mu = mean(lLRT_dif))

# Test de Wilcoxon

wilcox.test(lLRT_CM, lLRT_A, paired = TRUE, alternative = "two.sided", conf.int = TRUE)

# Diferencias CM y A 
nobm <- datos[datos$Etnia!="Blanco-Mestizo",]
LRT_dif <- nobm$LRT_CM - nobm$LRT_A
#length(LRT_dif)

# Como se ve graficamente, parece que no hay normalidad
hist(LRT_dif, freq = FALSE, main = 'Histograma de las la diferencia de mediciones LRT', ylab = 'Densidad', xlab = 'Diferenencia (cm)')
lines(density(LRT_dif), col = "red")
abline(v = median(LRT_dif, na.rm = TRUE), col = "red", lty = 2)
boxplot_dif <- boxplot(LRT_dif, main = "Boxplot diferencias LRT etnia NO Blanco-Mestizo") # No hay outliers

# Test Lilliefors

lillie.test(LRT_dif)

# Simetria

set.seed(123)
symmetry_test(LRT_dif, "MI", mu = mean(LRT_dif))

# Test de Wilcoxon

wilcox.test(nobm$LRT_CM, nobm$LRT_A, paired = TRUE, alternative = "two.sided", conf.int = TRUE)

# Longitud rodilla maleolo

# Comparar LRM 90 y R
boxplot(data.frame(datos$LRM_90, datos$LRM_R), ylab = 'Longitud Rodilla-Maleolo (cm)', xaxt = "n", main = "Boxplot Longitud rodilla-maléolo por tipo de medición")
axis(1, at = c(1, 2),
     labels = c("Pierna a 90°", "Pierna recta"))

# Diferencias CM y A
LRM_dif <- datos$LRM_90 - datos$LRM_R

# Como se ve graficamente, parece que no hay normalidad
hist(LRM_dif, freq = FALSE, main = 'Histograma de las diferencias de mediciones LRM', ylab = 'Densidad', xlab = 'Diferenencia (cm)')
lines(density(LRM_dif), col = "red")
abline(v = median(LRM_dif, na.rm = TRUE), col = "red", lty = 2)
boxplot_dif <- boxplot(LRM_dif, main = "Boxplot de las diferencias LRM") # hay muchos outliers

# Atipicos
atipicos <- datos[which(LRM_dif %in% boxplot_dif$out),]

atipicos <- atipicos |> as.data.frame()
atipicos |> gt() |> gtExtras::gt_theme_538() |> tab_options(table.font.size = 9)

LRM_dif <- datos$LRM_90 - datos$LRM_R

hist(LRM_dif, freq = FALSE, main = 'Histograma de las la diferencias LRM_90 - LRT_A', ylab = 'Densidad', xlab = 'Diferencia (cm)')
lines(density(LRM_dif), col = "red")
abline(v = mean(LRM_dif, na.rm = TRUE), col = "red", lty = 2)
boxplot_dif <- boxplot(LRM_dif, main = "Boxplot de las diferencias LRM_90 - LRT_A")

qqnorm(LRM_dif, main = "Q-Q plot de las diferencias")
qqline(LRM_dif, col = "red")

# Test Lilliefors

lillie.test(LRM_dif) 

# Simetria

set.seed(123)
symmetry_test(LRM_dif, "MI", mu = mean(LRM_dif))

# Transformacion logaritmica

lLRM_R <- log(datos$LRM_R)
lLRM_90 <- log(datos$LRM_90)

lLRM_dif <- lLRM_90 - lLRM_R

hist(lLRM_dif, freq = FALSE, main = 'Histograma de las la diferencias LRM_90 - LRM_R', ylab = 'Densidad', xlab = 'Diferencia (cm)')
lines(density(lLRM_dif), col = "red")
abline(v = mean(lLRM_dif, na.rm = TRUE), col = "red", lty = 2)
boxplot_dif <- boxplot(lLRM_dif, main = "Boxplot de las diferencias ln(LRM)")

# Test Lilliefors

lillie.test(lLRM_dif) 

# Simetria

set.seed(123)
symmetry_test(lLRM_dif, "MI", mu = mean(lLRM_dif))

# Test del signo

SIGN.test(lLRM_90, lLRM_R, paired = TRUE, alternative = "two.sided", conf.int = TRUE)

# Analisis distinguiendo por sexo

# Masculino

hombres <- datos[datos$Sexo == "Masculino",]
LRM_dif <- hombres$LRM_90 - hombres$LRM_R

hist(LRM_dif, freq = FALSE, main = 'Diferencia de mediciones LRT (hombres)', ylab = 'Densidad', xlab = 'Diferenencia (cm)')
lines(density(LRM_dif), col = "red")
abline(v = median(LRM_dif, na.rm = TRUE), col = "red", lty = 2)
boxplot_dif <- boxplot(LRM_dif, main = "Boxplot diferencias LRM sexo masculino")

# Test Lilliefors

lillie.test(LRM_dif)

# Prueba t

t.test(hombres$LRM_R, hombres$LRM_90, paired = TRUE)

# Femenino

mujeres <- datos[datos$Sexo == "Femenino",]
LRM_dif <- mujeres$LRM_90 - mujeres$LRM_R

# Como se ve graficamente, parece que no hay normalidad
hist(LRM_dif, freq = FALSE, main = 'Diferencia de mediciones LRT (mujeres)', ylab = 'Densidad', xlab = 'Diferenencia (cm)')
lines(density(LRM_dif), col = "red")
abline(v = median(LRM_dif, na.rm = TRUE), col = "red", lty = 2)
boxplot_dif <- boxplot(LRM_dif, main = "Boxplot diferencia LRM sexo femenino") # hay muchos outliers

# Test Lilliefors

lillie.test(LRM_dif)

# Simetria

set.seed(123)
symmetry_test(LRM_dif, "MI", mu = mean(LRM_dif))

# Test de Wilcoxon

wilcox.test(mujeres$LRM_90, mujeres$LRM_R, paired = TRUE, alternative = "two.sided", conf.int = TRUE)

# Analisis distinguiendo por etnia

bm <- datos[datos$Etnia=="Blanco-Mestizo",]
LRM_dif <- bm$LRM_90 - bm$LRM_R

# Como se ve graficamente, parece que no hay normalidad
hist(LRM_dif, freq = FALSE, main = 'Histograma de las la diferencia de mediciones LRT', ylab = 'Densidad', xlab = 'Diferenencia (cm)')
lines(density(LRM_dif), col = "red")
abline(v = median(LRM_dif, na.rm = TRUE), col = "red", lty = 2)
boxplot_dif <- boxplot(LRM_dif, main = "Boxplot diferencias LRM etnia Blanco-Mestizo") # hay muchos outliers

# Test Lilliefors

lillie.test(LRM_dif)

# Simetria

set.seed(123)
symmetry_test(LRM_dif, "MI", mu = mean(LRM_dif))

# Transformacion logaritmica

lLRM_R <- log(bm$LRM_R)
lLRM_90 <- log(bm$LRM_90)

lLRM_dif <- lLRM_90 - lLRM_R

hist(lLRM_dif, freq = FALSE, main = 'Histograma de las la diferencias LRM_90 - LRM_R', ylab = 'Densidad', xlab = 'Diferencia (cm)')
lines(density(lLRM_dif), col = "red")
abline(v = mean(lLRM_dif, na.rm = TRUE), col = "red", lty = 2)
boxplot_dif <- boxplot(lLRM_dif, main = "Boxplot de las diferencias ln(LRM) etnia Blanco-Mestizo")

# Test Lilliefors

lillie.test(lLRM_dif) 

# Simetria

set.seed(123)
symmetry_test(lLRM_dif, "MI", mu = mean(lLRM_dif))

# Test del signo

SIGN.test(lLRM_90, lLRM_R, paired = TRUE, alternative = "two.sided", conf.int = TRUE)

# Afrocolombiano

# Diferencias CM y A 
nobm <- datos[datos$Etnia!="Blanco-Mestizo",]
LRM_dif <- nobm$LRM_90 - nobm$LRM_R
#length(LRM_dif)

# Como se ve graficamente, parece que no hay normalidad
hist(LRM_dif, freq = FALSE, main = 'Histograma de las la diferencia de mediciones LRT', ylab = 'Densidad', xlab = 'Diferenencia (cm)')
lines(density(LRM_dif), col = "red")
abline(v = median(LRM_dif, na.rm = TRUE), col = "red", lty = 2)
boxplot_dif <- boxplot(LRM_dif, main = "Boxplot diferencias LRM etnia No Blanco-Mestizo") # No hay outliers

# Test Lilliefors

lillie.test(LRM_dif)

# Prueba t

t.test(nobm$LRM_90, nobm$LRM_R, paired = TRUE)

# Modelos ####

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
colnames(datos) <- c("Fecha", "Localidad", "Programa", "Unidad_atencion", 
                     "Sexo", "Edad", "Etnia", "Peso", "Talla",
                     "LRT_CM","LRT_A", "LRM_90","LRM_R")
datos$Sexo <- as.factor(datos$Sexo)
datos$Etnia <- as.factor(datos$Etnia)
datos <- datos[-which(datos$Edad<60),] # >60
datos <- datos |> filter(!(abs(LRT_A-LRT_CM) > 3 | abs(LRM_90-LRM_R) > 3))
datos <- datos |> as.data.frame()

# Modelo normal ####

regfit.full <- regsubsets(Talla ~ LRT_A + Edad + Sexo + Etnia,
                          datos, nbest = 1, nvmax = 10) # Mejor modelo de cada número de variables 

(reg.summary <- summary(regfit.full))

# BIC

reg.summary$bic

# Mejor modelo

which.min(reg.summary$bic)
plot(reg.summary$bic , xlab = "Number of Variables",
     ylab = "BIC", type = "l")
points(3, reg.summary$bic[3], col = "red", cex = 2,
       pch = 20)
plot(regfit.full , scale = "bic")

# Coeficientes

coef(regfit.full, 3)

# Cross validation

# Crear 5 folds
set.seed(123)  # Semilla para reproducibilidad
folds <- vfold_cv(datos, v = 10)

# Inicializar lista para almacenar errores
errores <- numeric(length(folds$splits))

# Loop para entrenar y validar el modelo en cada fold
for (i in seq_along(folds$splits)) {
  # Dividir datos
  train_data <- analysis(folds$splits[[i]])  # Datos de entrenamiento
  test_data <- assessment(folds$splits[[i]]) # Datos de validación
  
  # Ajustar el modelo
  modelo <- lm(Talla ~ LRT_A + Edad + Sexo, data = train_data)
  
  # Predicciones
  predicciones <- predict(modelo, newdata = test_data)
  
  # Calcular error cuadrático medio
  errores[i] <- mean((test_data$Talla - predicciones)^2)
}

# MSE

# Para ir almacenando los MSE de los modelos y las longitudes
MSEs <- data.frame(Modelo = character(),
                   Medida = character(),
                   MSE = numeric())

# Promedio del error cuadrático medio
mean(errores)

MSEs <- rbind(MSEs, data.frame(Modelo = "Normal", Medida = "LRT_A", MSE = mean(errores)))

# Mejor modelo con cinta metrica

regfit.full <- regsubsets(Talla ~ LRT_CM + Edad + Sexo + Etnia,
                          datos, nbest = 1, nvmax = 10) # Mejor modelo de cada número de variables 

(reg.summary <- summary(regfit.full))

# BIC

reg.summary$bic

# Mejor modelo

which.min(reg.summary$bic)
plot(reg.summary$bic , xlab = "Number of Variables",
     ylab = "BIC", type = "l")
points(3, reg.summary$bic[3], col = "red", cex = 2,
       pch = 20)
plot(regfit.full , scale = "bic")

# Coeficientes

coef(regfit.full, 3)

# Cross validation

# Crear 10 folds
set.seed(123)  # Semilla para reproducibilidad
folds <- vfold_cv(datos, v = 10)

# Inicializar lista para almacenar errores
errores <- numeric(length(folds$splits))

# Loop para entrenar y validar el modelo en cada fold
for (i in seq_along(folds$splits)) {
  # Dividir datos
  train_data <- analysis(folds$splits[[i]])  # Datos de entrenamiento
  test_data <- assessment(folds$splits[[i]]) # Datos de validación
  
  # Ajustar el modelo
  modelo <- lm(Talla ~ LRT_CM + Edad + Sexo, data = train_data)
  
  # Predicciones
  predicciones <- predict(modelo, newdata = test_data)
  
  # Calcular error cuadrático medio
  errores[i] <- mean((test_data$Talla - predicciones)^2)
}

# MSE

# Promedio del error cuadrático medio
mean(errores)

MSEs <- rbind(MSEs, data.frame(Modelo = "Normal", Medida = "LRT_CM", MSE = mean(errores)))

# Mejor modelo LRM pierna estirada

regfit.full <- regsubsets(Talla ~ LRM_R + Edad + Sexo + Etnia,
                          datos, nbest = 1, nvmax = 10) # Mejor modelo de cada número de variables 

(reg.summary <- summary(regfit.full))

# BIC

reg.summary$bic

# Mejor modelo

which.min(reg.summary$bic)
plot(reg.summary$bic , xlab = "Number of Variables",
     ylab = "BIC", type = "l")
points(3, reg.summary$bic[3], col = "red", cex = 2,
       pch = 20)
plot(regfit.full , scale = "bic")

# Coeficientes

coef(regfit.full, 3)

# Cross validation

# Crear 10 folds
set.seed(123)  # Semilla para reproducibilidad
folds <- vfold_cv(datos, v = 10)

# Inicializar lista para almacenar errores
errores <- numeric(length(folds$splits))

# Loop para entrenar y validar el modelo en cada fold
for (i in seq_along(folds$splits)) {
  # Dividir datos
  train_data <- analysis(folds$splits[[i]])  # Datos de entrenamiento
  test_data <- assessment(folds$splits[[i]]) # Datos de validación
  
  # Ajustar el modelo
  modelo <- lm(Talla ~ LRM_R + Edad + Sexo, data = train_data)
  
  # Predicciones
  predicciones <- predict(modelo, newdata = test_data)
  
  # Calcular error cuadrático medio
  errores[i] <- mean((test_data$Talla - predicciones)^2)
}

# MSE

# Promedio del error cuadrático medio
mean(errores)

MSEs <- rbind(MSEs, data.frame(Modelo = "Normal", Medida = "LRM_R", MSE = mean(errores))) # No se incluye pq aparentemente hay una leve homocedasticidad en los errores

# Mejor modelo LRM pierna a 90°

regfit.full <- regsubsets(Talla ~ LRM_90 + Edad + Sexo + Etnia,
                          datos, nbest = 1, nvmax = 10) # Mejor modelo de cada número de variables 

(reg.summary <- summary(regfit.full))

# BIC

reg.summary$bic

# Mejor modelo

which.min(reg.summary$bic)
plot(reg.summary$bic , xlab = "Number of Variables",
     ylab = "BIC", type = "l")
points(3, reg.summary$bic[3], col = "red", cex = 2,
       pch = 20)
plot(regfit.full , scale = "bic")

# Coeficientes

coef(regfit.full, 3)

# Cross validation

# Crear 10 folds
set.seed(123)  # Semilla para reproducibilidad
folds <- vfold_cv(datos, v = 10)

# Inicializar lista para almacenar errores
errores <- numeric(length(folds$splits))

# Loop para entrenar y validar el modelo en cada fold
for (i in seq_along(folds$splits)) {
  # Dividir datos
  train_data <- analysis(folds$splits[[i]])  # Datos de entrenamiento
  test_data <- assessment(folds$splits[[i]]) # Datos de validación
  
  # Ajustar el modelo
  modelo <- lm(Talla ~ LRM_90 + Edad + Sexo, data = train_data)
  
  # Predicciones
  predicciones <- predict(modelo, newdata = test_data)
  
  # Calcular error cuadrático medio
  errores[i] <- mean((test_data$Talla - predicciones)^2)
}

# MSE

# Promedio del error cuadrático medio
mean(errores)

MSEs <- rbind(MSEs, data.frame(Modelo = "Normal", Medida = "LRM_90", MSE = mean(errores))) # No se incluye pq el modelo no está bien especificado

# Resumen modelos normales

MSEs[1:4,]

# Evaluacion de supuestos

reg <- lm(Talla ~ LRT_A + Edad + Sexo, data = datos)
(summary.reg <- summary(reg))

# Linealidad

plot(reg, which = 1)

# Independencia de los errores

dwtest(reg)

# Normalidad de los errores

plot(reg, which = 2)
lillie.test(reg$residuals)
ad.test(reg$residuals)

# Homoscedasticidad

plot(reg, which = 3)
bptest(reg)

# Multicolinealidad

vif(reg)

# Modelo bien especificado

resettest(reg, type="regressor")
resettest(reg, type="fitted")

# Observaciones atípicas y de alto apalancamiento
  
plot(reg,which=5)
stud_res<-studres(reg)
# head(sort(abs(stud_res),decreasing=TRUE))
boxplot(stud_res)

n <- nrow(datos)
corte <- 4/(n-length(reg$coefficients)-2) #Es una regla usada en la práctica
plot(reg, which=4, cook.levels=corte)
abline(h=corte, lty=2, col="red")
cooksd<-cooks.distance(reg)
# cooksd[which(cooksd>corte)]

influencePlot(reg, id.method="identify", main="Gráfico de influencia", sub="El tamaño del círculo es proporcional a la D_Cook")

# Modelo sin obs influyentes

reg2 <- update(reg,subset={setdiff(row(datos)[,1], c(533,43,549))})
summary(reg2)

# Modelo Lasso ####

# LRT Antropómetro

x <- model.matrix(Talla ~ LRT_A + Edad + Sexo + Etnia, data = datos)[, -1]
y <- datos$Talla

grid <- 10^seq(10, -2, length = 100) # grilla para buscar el lambda que minimice el MSE

set.seed(123)
train <- sample(1:nrow(x), nrow(x)*0.9) # 90% entrenamiento
test <- (-train) # 10% para prueba
y.test <- y[test]

lasso.mod <- glmnet(x[train , ], y[train], alpha = 1, lambda = grid)
plot(lasso.mod)

# Cross validation

set.seed(123)
cv.out <- cv.glmnet(x[train, ], y[train], alpha = 1, nfolds = 10) # validación cruzada para buscar el lambda
plot(cv.out)
bestlam <- cv.out$lambda.min
lasso.pred <- predict(lasso.mod , s = bestlam, newx = x[test , ])

# MSE

mean((lasso.pred - y.test)^2) # MSE sobre los datos de prueba 

MSEs <- rbind(MSEs, data.frame(Modelo = "LASSO", Medida = "LRT_A", MSE = mean((lasso.pred - y.test)^2)))

out <- glmnet(x, y, alpha = 1, lambda = grid)
lasso.coef <- predict(out , type = "coefficients", s = bestlam)[1:7,]

# Coeficientes

lasso.coef

# LRT cinta métrica

x <- model.matrix(Talla ~ LRT_CM + Edad + Sexo + Etnia, data = datos)[, -1]
y <- datos$Talla

grid <- 10^seq(10, -2, length = 100) # grilla para buscar el lambda que minimice el MSE

set.seed(123)
train <- sample(1:nrow(x), nrow(x)*0.9) # 90% entrenamiento
test <- (-train) # 10% para prueba
y.test <- y[test]

lasso.mod <- glmnet(x[train , ], y[train], alpha = 1, lambda = grid)
plot(lasso.mod)

# Cross validation

set.seed(123)
cv.out <- cv.glmnet(x[train, ], y[train], alpha = 1, nfolds = 10) # validación cruzada para buscar el lambda
plot(cv.out)
bestlam <- cv.out$lambda.min
lasso.pred <- predict(lasso.mod , s = bestlam, newx = x[test , ])

# MSE

mean((lasso.pred - y.test)^2) # MSE sobre los datos de prueba 
MSEs <- rbind(MSEs, data.frame(Modelo = "LASSO", Medida = "LRT_CM", MSE = mean((lasso.pred - y.test)^2)))

out <- glmnet(x, y, alpha = 1, lambda = grid)
lasso.coef <- predict(out , type = "coefficients", s = bestlam)[1:7,]

# Coeficientes

lasso.coef

# LRM pierna estirada

x <- model.matrix(Talla ~ LRM_R + Edad + Sexo + Etnia, data = datos)[, -1]
y <- datos$Talla

grid <- 10^seq(10, -2, length = 100) # grilla para buscar el lambda que minimice el MSE

set.seed(123)
train <- sample(1:nrow(x), nrow(x)*0.9) # 90% entrenamiento
test <- (-train) # 10% para prueba
y.test <- y[test]

lasso.mod <- glmnet(x[train , ], y[train], alpha = 1, lambda = grid)
plot(lasso.mod)

# Cross validation

set.seed(123)
cv.out <- cv.glmnet(x[train, ], y[train], alpha = 1, nfolds = 10) # validación cruzada para buscar el lambda
plot(cv.out)
bestlam <- cv.out$lambda.min
lasso.pred <- predict(lasso.mod , s = bestlam, newx = x[test , ])

# MSE

mean((lasso.pred - y.test)^2) # MSE sobre los datos de prueba 

MSEs <- rbind(MSEs, data.frame(Modelo = "LASSO", Medida = "LRM_R", MSE = mean((lasso.pred - y.test)^2)))

out <- glmnet(x, y, alpha = 1, lambda = grid)
lasso.coef <- predict(out , type = "coefficients", s = bestlam)[1:7,]

# Coeficientes

lasso.coef

# LRM pierna a 90°

x <- model.matrix(Talla ~ LRM_90 + Edad + Sexo + Etnia, data = datos)[, -1]
y <- datos$Talla

grid <- 10^seq(10, -2, length = 100) # grilla para buscar el lambda que minimice el MSE

set.seed(123)
train <- sample(1:nrow(x), nrow(x)*0.9) # 90% entrenamiento
test <- (-train) # 10% para prueba
y.test <- y[test]

lasso.mod <- glmnet(x[train , ], y[train], alpha = 1, lambda = grid)
plot(lasso.mod)

# Cross validation

set.seed(123)
cv.out <- cv.glmnet(x[train, ], y[train], alpha = 1, nfolds = 10) # validación cruzada para buscar el lambda
plot(cv.out)
bestlam <- cv.out$lambda.min
lasso.pred <- predict(lasso.mod , s = bestlam, newx = x[test , ])

# MSE

mean((lasso.pred - y.test)^2) # MSE sobre los datos de prueba 

MSEs <- rbind(MSEs, data.frame(Modelo = "LASSO", Medida = "LRM_90", MSE = mean((lasso.pred - y.test)^2)))

out <- glmnet(x, y, alpha = 1, lambda = grid)
lasso.coef <- predict(out , type = "coefficients", s = bestlam)[1:7,]

# coeficientes

lasso.coef
lasso.coef[lasso.coef != 0]

# Resumen modelos LASSO

MSEs[1:8,]

# Modelo Gamma ####

# LRT Antropómetro

# Modelo nulo
gamma.reg_null <- glm(Talla ~ 1, family= Gamma(),data = datos)

# Estimacion del modelo
gamma.reg <- glm(Talla ~ LRT_A + Edad + Etnia + Sexo, family= Gamma(),data = datos)
summary(gamma.reg)

# Mejor modelo

stepwise <- stepAIC(gamma.reg_null, scope = list(lower = gamma.reg_null, upper = gamma.reg), trace = TRUE, k= 2, direction="forward") # k= log(nrow(datos)) - BIC
gamma.final <- glm(Talla ~ LRT_A + Edad + Sexo, family= Gamma(),data = datos)
summary(gamma.final)

# Cross validation

# k folds
best.fit <- pred <- CV.ERRORS <- cv.errors <- NULL

set.seed(123)  # Semilla para reproducibilidad
folds <- vfold_cv(datos, v = 10)

# Inicializar lista para almacenar errores
errores <- numeric(length(folds$splits))

# Loop para entrenar y validar el modelo en cada fold
for (i in seq_along(folds$splits)) {
  # Dividir datos
  train_data <- analysis(folds$splits[[i]])  # Datos de entrenamiento
  test_data <- assessment(folds$splits[[i]]) # Datos de validación
  
  # Ajustar el modelo
  modelo <- glm(Talla ~ LRT_A + Edad + Sexo, family= Gamma(), data = train_data)
  
  # Predicciones
  predicciones <- predict(modelo, newdata = test_data, type = "response")
  
  # Calcular error cuadrático medio
  errores[i] <- mean((test_data$Talla - predicciones)^2)
}

# Error cuadratico medio
mean(errores)

MSEs <- rbind(MSEs, data.frame(Modelo = "Gamma", Medida = "LRT_A", MSE = mean(errores)))

# LRT cinta métrica

# Modelo nulo
gamma.reg_null <- glm(Talla ~ 1, family= Gamma(),data = datos)

# Estimacion del modelo
gamma.reg <- glm(Talla ~ LRT_CM + Edad + Etnia + Sexo, family= Gamma(),data = datos)
summary(gamma.reg)

# Mejor modelo

stepwise <- stepAIC(gamma.reg_null, scope = list(lower = gamma.reg_null, upper = gamma.reg), trace = TRUE, k= 2, direction="forward") # k= log(nrow(datos)) - BIC

# Estimacion del modelo
gamma.final <- glm(Talla ~ LRT_CM + Edad + Sexo, family= Gamma(),data = datos)
summary(gamma.final)

# Cross validation

# k folds
best.fit <- pred <- CV.ERRORS <- cv.errors <- NULL

set.seed(123)  # Semilla para reproducibilidad
folds <- vfold_cv(datos, v = 10)

# Inicializar lista para almacenar errores
errores <- numeric(length(folds$splits))

# Loop para entrenar y validar el modelo en cada fold
for (i in seq_along(folds$splits)) {
  # Dividir datos
  train_data <- analysis(folds$splits[[i]])  # Datos de entrenamiento
  test_data <- assessment(folds$splits[[i]]) # Datos de validación
  
  # Ajustar el modelo
  modelo <- glm(Talla ~ LRT_CM + Edad + Sexo, family= Gamma(), data = train_data)
  
  # Predicciones
  predicciones <- predict(modelo, newdata = test_data, type = "response")
  
  # Calcular error cuadrático medio
  errores[i] <- mean((test_data$Talla - predicciones)^2)
}

# Error cuadratico medio
mean(errores)

MSEs <- rbind(MSEs, data.frame(Modelo = "Gamma", Medida = "LRT_CM", MSE = mean(errores)))

# LRM pierna estirada

# Modelo nulo
gamma.reg_null <- glm(Talla ~ 1, family= Gamma(),data = datos)

# Estimacion del modelo
gamma.reg <- glm(Talla ~ LRM_R + Edad + Etnia + Sexo, family= Gamma(),data = datos)
summary(gamma.reg)

# Mejor modelo

stepwise <- stepAIC(gamma.reg_null, scope = list(lower = gamma.reg_null, upper = gamma.reg), trace = TRUE, k= 2, direction="forward") # k= log(nrow(datos)) - BIC

# Estimacion del modelo
gamma.final <- glm(Talla ~ LRM_R + Edad + Sexo, family= Gamma(),data = datos)
summary(gamma.final)

# Cross validation

# k folds
best.fit <- pred <- CV.ERRORS <- cv.errors <- NULL

set.seed(123)  # Semilla para reproducibilidad
folds <- vfold_cv(datos, v = 10)

# Inicializar lista para almacenar errores
errores <- numeric(length(folds$splits))

# Loop para entrenar y validar el modelo en cada fold
for (i in seq_along(folds$splits)) {
  # Dividir datos
  train_data <- analysis(folds$splits[[i]])  # Datos de entrenamiento
  test_data <- assessment(folds$splits[[i]]) # Datos de validación
  
  # Ajustar el modelo
  modelo <- glm(Talla ~ LRM_R + Edad + Sexo, family= Gamma(), data = train_data)
  
  # Predicciones
  predicciones <- predict(modelo, newdata = test_data, type = "response")
  
  # Calcular error cuadrático medio
  errores[i] <- mean((test_data$Talla - predicciones)^2)
}

# Error cuadratico medio
mean(errores)

MSEs <- rbind(MSEs, data.frame(Modelo = "Gamma", Medida = "LRM_R", MSE = mean(errores)))

# LRM pierna 90°

# Modelo nulo
gamma.reg_null <- glm(Talla ~ 1, family= Gamma(),data = datos)

# Estimacion del modelo
gamma.reg <- glm(Talla ~ LRM_90 + Edad + Etnia + Sexo, family= Gamma(),data = datos)
summary(gamma.reg)

# Mejor modelo

stepwise <- stepAIC(gamma.reg_null, scope = list(lower = gamma.reg_null, upper = gamma.reg), trace = TRUE, k= 2, direction="forward") # k= log(nrow(datos)) - BIC

# Estimacion del modelo
gamma.final <- glm(Talla ~ LRM_90 + Edad + Sexo, family= Gamma(),data = datos)
summary(gamma.final)

# Cross validation

# k folds
best.fit <- pred <- CV.ERRORS <- cv.errors <- NULL

set.seed(123)  # Semilla para reproducibilidad
folds <- vfold_cv(datos, v = 10)

# Inicializar lista para almacenar errores
errores <- numeric(length(folds$splits))

# Loop para entrenar y validar el modelo en cada fold
for (i in seq_along(folds$splits)) {
  # Dividir datos
  train_data <- analysis(folds$splits[[i]])  # Datos de entrenamiento
  test_data <- assessment(folds$splits[[i]]) # Datos de validación
  
  # Ajustar el modelo
  modelo <- glm(Talla ~ LRM_90 + Edad + Sexo, family= Gamma(), data = train_data)
  
  # Predicciones
  predicciones <- predict(modelo, newdata = test_data, type = "response")
  
  # Calcular error cuadrático medio
  errores[i] <- mean((test_data$Talla - predicciones)^2)
}

# Error cuadratico medio
mean(errores)

MSEs <- rbind(MSEs, data.frame(Modelo = "Gamma", Medida = "LRM_90", MSE = mean(errores)))

# Resumen modelos Gamma

MSEs[1:12,]

# Fórmulas Benjumea ####

# LRT Antropómetro

# LRT_A
datos$Benjumea_LRT_A = rep(NA,nrow(datos))

# Indigena masculino
datos$Benjumea_LRT_A = ifelse(datos$Sexo=="Masculino" & datos$Etnia == "Indigena", 82.695 + 1.745*datos$LRT_A - 0.121*datos$Edad, datos$Benjumea_LRT_A)
# Indigena femenino
datos$Benjumea_LRT_A = ifelse(datos$Sexo!="Masculino" & datos$Etnia == "Indigena", 90.281 + 1.436*datos$LRT_A - 0.102*datos$Edad, datos$Benjumea_LRT_A)
# Afrodescendiente masculino
datos$Benjumea_LRT_A = ifelse(datos$Sexo=="Masculino" & datos$Etnia == "Afrocolombiano", 79.298 + 1.855*datos$LRT_A - 0.141*datos$Edad, datos$Benjumea_LRT_A)
# Afrodescendiente femenino
datos$Benjumea_LRT_A = ifelse(datos$Sexo!="Masculino" & datos$Etnia == "Afrocolombiano", 76.233 + 1.767*datos$LRT_A - 0.098*datos$Edad, datos$Benjumea_LRT_A)
# Blanco-Mestizo masculino
datos$Benjumea_LRT_A = ifelse(datos$Sexo=="Masculino" & datos$Etnia == "Blanco-Mestizo", 75.514 + 1.883*datos$LRT_A - 0.108*datos$Edad, datos$Benjumea_LRT_A)
# Blanco-Mestizo femenino
datos$Benjumea_LRT_A = ifelse(datos$Sexo!="Masculino" & datos$Etnia == "Blanco-Mestizo", 86.497 + 1.553*datos$LRT_A - 0.119*datos$Edad, datos$Benjumea_LRT_A)

mean((datos$Talla - datos$Benjumea_LRT_A)^2, na.rm = TRUE)

MSEs <- rbind(MSEs, data.frame(Modelo = "Benjumea", Medida = "LRT_A", MSE = mean((datos$Talla - datos$Benjumea_LRT_A)^2, na.rm = TRUE)))

# LRT cinta métrica

# LRT_CM
datos$Benjumea_LRT_CM = rep(NA,nrow(datos))

# Indigena masculino
datos$Benjumea_LRT_CM = ifelse(datos$Sexo=="Masculino" & datos$Etnia == "Indigena", 82.695 + 1.745*datos$LRT_CM - 0.121*datos$Edad, datos$Benjumea_LRT_CM)
# Indigena femenino
datos$Benjumea_LRT_CM = ifelse(datos$Sexo!="Masculino" & datos$Etnia == "Indigena", 90.281 + 1.436*datos$LRT_CM - 0.102*datos$Edad, datos$Benjumea_LRT_CM)
# Afrodescendiente masculino
datos$Benjumea_LRT_CM = ifelse(datos$Sexo=="Masculino" & datos$Etnia == "Afrocolombiano", 79.298 + 1.855*datos$LRT_CM - 0.141*datos$Edad, datos$Benjumea_LRT_CM)
# Afrodescendiente femenino
datos$Benjumea_LRT_CM = ifelse(datos$Sexo!="Masculino" & datos$Etnia == "Afrocolombiano", 76.233 + 1.767*datos$LRT_CM - 0.098*datos$Edad, datos$Benjumea_LRT_CM)
# Blanco-Mestizo masculino
datos$Benjumea_LRT_CM = ifelse(datos$Sexo=="Masculino" & datos$Etnia == "Blanco-Mestizo", 75.514 + 1.883*datos$LRT_CM - 0.108*datos$Edad, datos$Benjumea_LRT_CM)
# Blanco-Mestizo femenino
datos$Benjumea_LRT_CM = ifelse(datos$Sexo!="Masculino" & datos$Etnia == "Blanco-Mestizo", 86.497 + 1.553*datos$LRT_CM - 0.119*datos$Edad, datos$Benjumea_LRT_CM)

mean((datos$Talla - datos$Benjumea_LRT_CM)^2, na.rm = TRUE)

MSEs <- rbind(MSEs, data.frame(Modelo = "Benjumea", Medida = "LRT_CM", MSE = mean((datos$Talla - datos$Benjumea_LRT_CM)^2, na.rm = TRUE)))

# Resumen fórmulas Benjumea

MSEs[13:14,]

# Fórmulas Arango y Zamora ####

# LRM_R
datos$Arango_LRM_R = rep(NA,nrow(datos))

# Masculino
datos$Arango_LRM_R = ifelse(datos$Sexo=="Masculino", 119.6 + 1.121*datos$LRM_R - 0.117*datos$Edad, datos$Arango_LRM_R)
# Femenino
datos$Arango_LRM_R = ifelse(datos$Sexo!="Masculino", 107.7 + 1.263*datos$LRM_R - 0.159*datos$Edad, datos$Arango_LRM_R)

mean((datos$Talla - datos$Arango_LRM_R)^2, na.rm = TRUE)

MSEs <- rbind(MSEs, data.frame(Modelo = "Arango, Zamora", Medida = "LRT_CM", MSE = mean((datos$Talla - datos$Arango_LRM_R)^2, na.rm = TRUE)))

# LRM pierna a 90°

# LRM_90
datos$Arango_LRM_90 = rep(NA,nrow(datos))

# Masculino
datos$Arango_LRM_90 = ifelse(datos$Sexo=="Masculino", 119.6 + 1.121*datos$LRM_90 - 0.117*datos$Edad, datos$Arango_LRM_90)
# Femenino
datos$Arango_LRM_90 = ifelse(datos$Sexo!="Masculino", 107.7 + 1.263*datos$LRM_90 - 0.159*datos$Edad, datos$Arango_LRM_90)

mean((datos$Talla - datos$Arango_LRM_90)^2, na.rm = TRUE)

MSEs <- rbind(MSEs, data.frame(Modelo = "Arango, Zamora", Medida = "LRT_CM", MSE = mean((datos$Talla - datos$Arango_LRM_90)^2, na.rm = TRUE)))

# Resumen fórmulas Arango y Zamora

MSEs[15:16,]

# Tabla comparando todos los modelos

MSEs

