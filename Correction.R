##################################################
# PESO
##################################################

base_datos <- read.csv("**/base_datos_talla_peso.csv")

names(base_datos)

base_datos_sinajustar <- subset(base_datos, select = c(5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21)) 

names(base_datos_sinajustar)

write.csv(base_datos_sinajustar, "**/base_datos_sinajustar_peso.csv", row.names = FALSE)

############################################

nous_pesos <- base_datos[base_datos$clase == "CLASE2",]

sd(nous_pesos$PAPP.A..0.2.3.0.)
sd(nous_pesos$hCG.0.3.5.0.)

plot (nous_pesos$PESO, nous_pesos$PAPP.A..0.2.3.0., main = "Peso vs PAPP")
plot (nous_pesos$PESO, nous_pesos$hCG.0.3.5.0., ylim=c(-1,1), main = "Peso vs hCG")

linear_model_PAPP <- lm(PAPP.A..0.2.3.0. ~ PESO, data = nous_pesos)
summary(linear_model_PAPP)

linear_model_hCG <- lm(hCG.0.3.5.0. ~ PESO, data = nous_pesos)
summary(linear_model_hCG)

nous_pesos$PAPP.A..0.2.3.0. <- log10(10^((nous_pesos$PAPP.A..0.2.3.0.))/10^((0.4293679-0.0066291*nous_pesos$PESO)))
nous_pesos$hCG.0.3.5.0. <- log10(10^((nous_pesos$hCG.0.3.5.0.))/10^((0.2556103-0.0041703*nous_pesos$PESO)))

names(nous_pesos)

sd(nous_pesos$PAPP.A..0.2.3.0.)
sd(nous_pesos$hCG.0.3.5.0.)

base_datos_clase2 <- subset(nous_pesos, select = c(5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21))

names(base_datos_clase2)

base_datos_clase1_peso_talla <- base_datos[base_datos$clase == "CLASE1",]

names(base_datos_clase1_peso_talla)

base_datos_clase1 <- subset(base_datos_clase1_peso_talla, select = c(5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21))

names(base_datos_clase1)

names(base_datos_clase2)

base_datos_ajustados_peso <- rbind(base_datos_clase1,base_datos_clase2)

write.csv(base_datos_ajustados, "**/base_datos_ajustados_peso.csv", row.names = FALSE)

##################################################
# TALLA
##################################################

base_datos <- read.csv("**/base_datos_talla_peso.csv")

NA_values <- !is.na(base_datos$TALLA)
base_datos_sinajustar_pre <- base_datos[NA_values,]

names(base_datos_sinajustar_pre)

base_datos_sinajustar <- subset(base_datos_sinajustar_pre, select = c(5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21)) 

names(base_datos_sinajustar)

write.csv(base_datos_sinajustar, "**/base_datos_sinajustar_talla.csv", row.names = FALSE)

############################################

nous_pesos <- base_datos_sinajustar_pre[base_datos_sinajustar_pre$clase == "CLASE2",]

sd(nous_pesos$PAPP.A..0.2.3.0.)
sd(nous_pesos$hCG.0.3.5.0.)

plot (nous_pesos$TALLA, nous_pesos$PAPP.A..0.2.3.0., main = "Talla vs PAPP")
plot (nous_pesos$TALLA, nous_pesos$hCG.0.3.5.0., ylim=c(-1,1), main = "Talla vs hCG")

linear_model_PAPP <- lm(nous_pesos$PAPP.A..0.2.3.0. ~ nous_pesos$TALLA)
summary(linear_model_PAPP)

linear_model_hCG <- lm(nous_pesos$hCG.0.3.5.0. ~ nous_pesos$TALLA)
summary(linear_model_hCG)

nous_pesos$PAPP.A..0.2.3.0. <- log10(10^((nous_pesos$PAPP.A..0.2.3.0.))/10^((0.6247007-0.0038604*nous_pesos$TALLA)))

nous_pesos$hCG.0.3.5.0. <- log10(10^((nous_pesos$hCG.0.3.5.0.))/10^((0.1441819-0.0009196*nous_pesos$TALLA)))

names(nous_pesos)

sd(nous_pesos$PAPP.A..0.2.3.0.)
sd(nous_pesos$hCG.0.3.5.0.)

base_datos_clase2 <- subset(nous_pesos, select = c(5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21))

names(base_datos_clase2)

base_datos_clase1_peso_talla <- base_datos[base_datos$clase == "CLASE1",]

names(base_datos_clase1_peso_talla)

base_datos_clase1 <- subset(base_datos_clase1_peso_talla, select = c(5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21))

names(base_datos_clase1)

names(base_datos_clase2)

base_datos_ajustados <- rbind(base_datos_clase1,base_datos_clase2)

write.csv(base_datos_ajustados, "**/base_datos_ajustados_talla.csv", row.names = FALSE)

##################################################
# IMC
##################################################

base_datos <- read.csv("**/base_datos_talla_peso.csv")

NA_values <- !is.na(base_datos$TALLA)
base_datos_sinajustar_pre <- base_datos[NA_values,]

names(base_datos_sinajustar_pre)

base_datos_sinajustar <- subset(base_datos_sinajustar_pre, select = c(5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21)) 

names(base_datos_sinajustar)

write.csv(base_datos_sinajustar, "**/base_datos_sinajustar_IMC.csv", row.names = FALSE)

############################################

nous_pesos <- base_datos_sinajustar_pre[base_datos_sinajustar_pre$clase == "CLASE2",]

sd(nous_pesos$PAPP.A..0.2.3.0.)
sd(nous_pesos$hCG.0.3.5.0.)

nous_pesos$IMC <- (nous_pesos$PESO)/(nous_pesos$TALLA/100)^2

plot (nous_pesos$IMC, nous_pesos$PAPP.A..0.2.3.0., main = "IMC vs PAPP")
plot (nous_pesos$IMC, nous_pesos$hCG.0.3.5.0., ylim=c(-1,1), main = "IMC vs hCG")

linear_model_PAPP <- lm(nous_pesos$PAPP.A..0.2.3.0. ~ nous_pesos$IMC)
summary(linear_model_PAPP)

linear_model_hCG <- lm(nous_pesos$hCG.0.3.5.0. ~ nous_pesos$IMC)
summary(linear_model_hCG)

nous_pesos$PAPP.A..0.2.3.0. <- log10(10^((nous_pesos$PAPP.A..0.2.3.0.))/10^((0.0284344-0.0013367*nous_pesos$IMC)))

nous_pesos$hCG.0.3.5.0. <- log10(10^((nous_pesos$hCG.0.3.5.0.))/10^((0.0121903-0.0007219*nous_pesos$IMC)))

names(nous_pesos)

sd(nous_pesos$PAPP.A..0.2.3.0.)
sd(nous_pesos$hCG.0.3.5.0.)

base_datos_clase2 <- subset(nous_pesos, select = c(5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21))

names(base_datos_clase2)

base_datos_clase1_peso_talla <- base_datos[base_datos$clase == "CLASE1",]

names(base_datos_clase1_peso_talla)

base_datos_clase1 <- subset(base_datos_clase1_peso_talla, select = c(5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21))

names(base_datos_clase1)

names(base_datos_clase2)

base_datos_ajustados <- rbind(base_datos_clase1,base_datos_clase2)

write.csv(base_datos_ajustados, "**/base_datos_ajustados_IMC.csv", row.names = FALSE)

##################################################
# IMC tramos
##################################################

base_datos <- read.csv("**/base_datos_talla_peso.csv")

NA_values <- !is.na(base_datos$TALLA)
base_datos_sinajustar_pre <- base_datos[NA_values,]

names(base_datos_sinajustar_pre)

base_datos_sinajustar <- subset(base_datos_sinajustar_pre, select = c(5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21)) 

names(base_datos_sinajustar)

write.csv(base_datos_sinajustar, "**/base_datos_sinajustar_IMC_tramos.csv", row.names = FALSE)

############################################

nous_pesos <- base_datos_sinajustar_pre[base_datos_sinajustar_pre$clase == "CLASE2",]

sd(nous_pesos$PAPP.A..0.2.3.0.)
sd(nous_pesos$hCG.0.3.5.0.)

nous_pesos$IMC <- (nous_pesos$PESO)/(nous_pesos$TALLA/100)^2

plot(nous_pesos[nous_pesos$IMC<=20,]$PESO, nous_pesos[nous_pesos$IMC<=20,]$PAPP.A..0.2.3.0., ylim=c(-1,1), main = "IMC <= 20 vs PAPP")
plot(nous_pesos[nous_pesos$IMC > 20 & nous_pesos$IMC <= 25,]$PESO, nous_pesos[nous_pesos$IMC > 20 & nous_pesos$IMC <= 25,]$PAPP.A..0.2.3.0., ylim=c(-1,1), main = "IMC > 20 & IMC <= 25 vs PAPP")
plot(nous_pesos[nous_pesos$IMC > 25 & nous_pesos$IMC <= 30,]$PESO, nous_pesos[nous_pesos$IMC > 25 & nous_pesos$IMC <= 30,]$PAPP.A..0.2.3.0., ylim=c(-1,1), main = "IMC > 25 & IMC <= 30 vs PAPP")
plot(nous_pesos[nous_pesos$IMC > 30,]$PESO, nous_pesos[nous_pesos$IMC > 30,]$PAPP.A..0.2.3.0., ylim=c(-1,1), main = "IMC > 30 vs PAPP")

plot(nous_pesos[nous_pesos$IMC<=20,]$PESO, nous_pesos[nous_pesos$IMC<=20,]$hCG.0.3.5.0., ylim=c(-1,1), main = "IMC <= 20 vs hCG")
plot(nous_pesos[nous_pesos$IMC > 20 & nous_pesos$IMC <= 25,]$PESO, nous_pesos[nous_pesos$IMC > 20 & nous_pesos$IMC <= 25,]$hCG.0.3.5.0., ylim=c(-1,1), main = "IMC > 20 & IMC <= 25 vs hCG")
plot(nous_pesos[nous_pesos$IMC > 25 & nous_pesos$IMC <= 30,]$PESO, nous_pesos[nous_pesos$IMC > 25 & nous_pesos$IMC <= 30,]$hCG.0.3.5.0., ylim=c(-1,1), main = "IMC > 25 & IMC <= 30 vs hCG")
plot(nous_pesos[nous_pesos$IMC > 30,]$PESO, nous_pesos[nous_pesos$IMC > 30,]$hCG.0.3.5.0., ylim=c(-1,1), main = "IMC > 30 vs hCG")

linear_model_PAPP_1 <- lm(nous_pesos[nous_pesos$IMC <= 20,]$PAPP.A..0.2.3.0. ~ nous_pesos[nous_pesos$IMC <= 20,]$PESO)
summary(linear_model_PAPP_1)

linear_model_PAPP_2 <- lm(nous_pesos[nous_pesos$IMC > 20 & nous_pesos$IMC <= 25,]$PAPP.A..0.2.3.0. ~ nous_pesos[nous_pesos$IMC > 20 & nous_pesos$IMC <= 25,]$PESO)
summary(linear_model_PAPP_2)

linear_model_PAPP_3 <- lm(nous_pesos[nous_pesos$IMC > 25 & nous_pesos$IMC <= 30,]$PAPP.A..0.2.3.0. ~ nous_pesos[nous_pesos$IMC > 25 & nous_pesos$IMC <= 30,]$PESO)
summary(linear_model_PAPP_3)

linear_model_PAPP_4 <- lm(nous_pesos[nous_pesos$IMC > 30,]$PAPP.A..0.2.3.0. ~ nous_pesos[nous_pesos$IMC > 30,]$PESO)
summary(linear_model_PAPP_4)

linear_model_hCG_1 <- lm(nous_pesos[nous_pesos$IMC <= 20,]$hCG.0.3.5.0. ~ nous_pesos[nous_pesos$IMC <= 20,]$PESO)
summary(linear_model_hCG_1)

linear_model_hCG_2 <- lm(nous_pesos[nous_pesos$IMC > 20 & nous_pesos$IMC <= 25,]$hCG.0.3.5.0. ~ nous_pesos[nous_pesos$IMC > 20 & nous_pesos$IMC <= 25,]$PESO)
summary(linear_model_hCG_2)

linear_model_hCG_3 <- lm(nous_pesos[nous_pesos$IMC > 25 & nous_pesos$IMC <= 30,]$hCG.0.3.5.0. ~ nous_pesos[nous_pesos$IMC > 25 & nous_pesos$IMC <= 30,]$PESO)
summary(linear_model_hCG_3)

linear_model_hCG_4 <- lm(nous_pesos[nous_pesos$IMC > 30,]$hCG.0.3.5.0. ~ nous_pesos[nous_pesos$IMC > 30,]$PESO)
summary(linear_model_hCG_4)

nous_pesos[nous_pesos$IMC <= 20,]$PAPP.A..0.2.3.0. <- log10(10^((nous_pesos[nous_pesos$IMC <= 20,]$PAPP.A..0.2.3.0.))/10^((0.696127-0.011873*nous_pesos[nous_pesos$IMC <= 20,]$PESO)))
nous_pesos[nous_pesos$IMC > 20 & nous_pesos$IMC <= 25,]$PAPP.A..0.2.3.0. <- log10(10^((nous_pesos[nous_pesos$IMC > 20 & nous_pesos$IMC <= 25,]$PAPP.A..0.2.3.0.))/10^((0.287087-0.004255*nous_pesos[nous_pesos$IMC > 20 & nous_pesos$IMC <= 25,]$PESO)))
nous_pesos[nous_pesos$IMC > 25 & nous_pesos$IMC <= 30,]$PAPP.A..0.2.3.0. <- log10(10^((nous_pesos[nous_pesos$IMC > 25 & nous_pesos$IMC <= 30,]$PAPP.A..0.2.3.0.))/10^((0.362574-0.005753*nous_pesos[nous_pesos$IMC > 25 & nous_pesos$IMC <= 30,]$PESO)))
nous_pesos[nous_pesos$IMC > 30,]$PAPP.A..0.2.3.0. <- log10(10^((nous_pesos[nous_pesos$IMC > 30,]$PAPP.A..0.2.3.0.))/10^((-0.0063834-0.0018696*nous_pesos[nous_pesos$IMC > 30,]$PESO)))

nous_pesos[nous_pesos$IMC <= 20,]$hCG.0.3.5.0. <- log10(10^((nous_pesos[nous_pesos$IMC <= 20,]$hCG.0.3.5.0.))/10^((0.0918438-0.0002075*nous_pesos[nous_pesos$IMC <= 20,]$PESO)))
nous_pesos[nous_pesos$IMC > 20 & nous_pesos$IMC <= 25,]$hCG.0.3.5.0. <- log10(10^((nous_pesos[nous_pesos$IMC > 20 & nous_pesos$IMC <= 25,]$hCG.0.3.5.0.))/10^((0.246390-0.003586*nous_pesos[nous_pesos$IMC > 20 & nous_pesos$IMC <= 25,]$PESO)))
nous_pesos[nous_pesos$IMC > 25 & nous_pesos$IMC <= 30,]$hCG.0.3.5.0. <- log10(10^((nous_pesos[nous_pesos$IMC > 25 & nous_pesos$IMC <= 30,]$hCG.0.3.5.0.))/10^((0.254950-0.004542*nous_pesos[nous_pesos$IMC > 25 & nous_pesos$IMC <= 30,]$PESO)))
nous_pesos[nous_pesos$IMC > 30,]$hCG.0.3.5.0. <- log10(10^((nous_pesos[nous_pesos$IMC > 30,]$hCG.0.3.5.0.))/10^((-0.0541398-0.0008137*nous_pesos[nous_pesos$IMC > 30,]$PESO)))

names(nous_pesos)

sd(nous_pesos$PAPP.A..0.2.3.0.)
sd(nous_pesos$hCG.0.3.5.0.)

base_datos_clase2 <- subset(nous_pesos, select = c(5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21))

names(base_datos_clase2)

base_datos_clase1_peso_talla <- base_datos[base_datos$clase == "CLASE1",]

names(base_datos_clase1_peso_talla)

base_datos_clase1 <- subset(base_datos_clase1_peso_talla, select = c(5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21))

names(base_datos_clase1)

names(base_datos_clase2)

base_datos_ajustados <- rbind(base_datos_clase1,base_datos_clase2)

write.csv(base_datos_ajustados, "**/base_datos_ajustados_IMC_tramos.csv", row.names = FALSE)

##################################################
# Peso y Talla
##################################################

base_datos <- read.csv("**/base_datos_talla_peso.csv")

NA_values <- !is.na(base_datos$TALLA)
base_datos_sinajustar_pre <- base_datos[NA_values,]

names(base_datos_sinajustar_pre)

base_datos_sinajustar <- subset(base_datos_sinajustar_pre, select = c(5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21)) 

names(base_datos_sinajustar)

write.csv(base_datos_sinajustar, "**/base_datos_sinajustar_peso_talla.csv", row.names = FALSE)

############################################

nous_pesos <- base_datos_sinajustar_pre[base_datos_sinajustar_pre$clase == "CLASE2",]

sd(nous_pesos$PAPP.A..0.2.3.0.)
sd(nous_pesos$hCG.0.3.5.0.)

plot (nous_pesos$PESO, nous_pesos$PAPP.A..0.2.3.0., ylim=c(-1,1), main = "Peso vs PAPP")
plot (nous_pesos$PESO, nous_pesos$hCG.0.3.5.0., ylim=c(-1,1), main = "Peso vs hCG")
plot (nous_pesos$TALLA, nous_pesos$PAPP.A..0.2.3.0., ylim=c(-1,1), main = "Talla vs PAPP")
plot (nous_pesos$TALLA, nous_pesos$hCG.0.3.5.0., ylim=c(-1,1), main = "Talla vs hCG")

linear_model_PAPP <- lm(PAPP.A..0.2.3.0. ~ PESO + TALLA, data = nous_pesos)
summary(linear_model_PAPP)

linear_model_hCG <- lm(hCG.0.3.5.0. ~ PESO + TALLA, data = nous_pesos)
summary(linear_model_hCG)

nous_pesos$PAPP.A..0.2.3.0. <- log10(10^((nous_pesos$PAPP.A..0.2.3.0.))/10^((0.5117503-0.0056715*nous_pesos$PESO-0.0008881*nous_pesos$TALLA)))
nous_pesos$hCG.0.3.5.0. <- log10(10^((nous_pesos$hCG.0.3.5.0.))/10^((0.0517490-0.0046413*nous_pesos$PESO+0.0015128*nous_pesos$TALLA)))

names(nous_pesos)

sd(nous_pesos$PAPP.A..0.2.3.0.)
sd(nous_pesos$hCG.0.3.5.0.)

base_datos_clase2 <- subset(nous_pesos, select = c(5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21))

names(base_datos_clase2)

base_datos_clase1_peso_talla <- base_datos_sinajustar_pre[base_datos_sinajustar_pre$clase == "CLASE1",]

names(base_datos_clase1_peso_talla)

base_datos_clase1 <- subset(base_datos_clase1_peso_talla, select = c(5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21))

names(base_datos_clase1)

names(base_datos_clase2)

base_datos_ajustados <- rbind(base_datos_clase1,base_datos_clase2)

write.csv(base_datos_ajustados, "**/base_datos_ajustados_peso_talla.csv", row.names = FALSE)
